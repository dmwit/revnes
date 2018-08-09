{-# LANGUAGE LambdaCase #-}
-- For now, this module is implemented in an inefficient way. There's lots of
-- low-hanging fruit for improving the performance without changing the API too
-- much.

-- | The top-level type in here is 'Folds'. It tracks a collection of (nested)
-- summaries of an underlying byte sequence. A closed fold displays the
-- summary, and any enclosed folds (or unfolded bytes) are hidden. An open fold
-- is \"transparent\": enclosed folds and bytes are shown instead of the
-- summary.
--
-- This simple explanation is complicated somewhat by the fact that the bytes
-- you see at a given memory address in the NES can change during the run of a
-- program. We'd like to be able to summarize a chunk of a PRG ROM, and have
-- that summary appear anywhere the ROM is mapped. We'd also like summaries to
-- be allowed to span across ROM boundaries (since, after all, there's no
-- reason to believe the program text breaks evenly on ROM boundaries).
-- Guaranteeing that summaries are nested and there's no ambiguity about which
-- summary is the right one to show, even as the memory map changes out from
-- under us, is quite tricky.
module RevNES.Fold
	( FMF, singleton, fromList, flatten, lengthFMF
	, Folds, addFold, removeFold, view
	) where

import Control.Monad
import Data.Default
import Data.Foldable
import Data.List
import Data.IntervalMap.Generic.Lazy (IntervalMap)
import Data.Map (Map)
import Data.Monoid
import Data.Ord
import Data.Set (Set)
import Data.Tree (Forest, Tree(..))
import RevNES.Slice hiding (singleton)
import RevNES.MemMap

import qualified Data.IntervalMap.Generic.Lazy as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified RevNES.Slice as Slice
import qualified RevNES.MemMap as MM

-- | Notionally, this is a @[(ChunkID, Word64)]@ which specifies a map from
-- 'Int's (e.g. offsets from some base CPU memory address) to a specific byte
-- in a 'Chunk' (which are the bytes you would see if you read from the
-- associated addresses). However, it's common to have 4KB chunks mapped
-- together, so we run-length encode, and the 'Monoid' instance takes care of
-- keeping the encoding invariant that neighboring elements with equal
-- 'ChunkID's are collapsed.
--
-- It's short for \"flattened 'MemMap' fragment\" -- flattened because there is
-- no mirroring, fragment because it's required to talk about every address in
-- the range and so there will be in general many fragments in disparate parts
-- of the NES' address space.
newtype FMF = FMF [(ChunkID, SliceU)] deriving (Eq, Show)
-- Invariant: adjacent elements can't be collapsed, that is, they either have
-- different 'ChunkID's or the end of the first one's slice is not one smaller
-- than the start of the second one's slice.

-- | @a <= a <\> b@; otherwise lexicographic
instance Ord FMF where
	compare l r = case sharedPrefix l r of
		(_, FMF [], FMF []) -> EQ
		(_, FMF [], _) -> LT
		(_, _, FMF []) -> GT
		(_, FMF (v:_), FMF (v':_)) -> compare v v'

instance Monoid FMF where
	mempty = FMF []
	mappend (FMF vs) (FMF vs') = FMF (go vs vs') where
		go [] vs = vs
		go [v@(chunk, slice)] ((v'@(chunk', slice')):rest) = (++rest)
			$ (if chunk == chunk' && end slice + 1 == start slice'
			   then [(chunk, endFromSizem1U (sizem1U slice + sizem1U slice' + 1) slice)]
			   else [v, v']
			  )
		go (v:vs) vs' = v:go vs vs'

singleton :: (ChunkID, SliceU) -> FMF
singleton x = FMF [x]

fromList :: [(ChunkID, SliceU)] -> FMF
fromList = foldMap singleton

-- | Given a memory map (typically the result of 'cpu' or 'ppu') and a chunk of
-- the memory to view, produce flattened fragments giving the source for each
-- byte that is actually backed by a 'Source'. The 'Word16's in the result are
-- addresses in the same address space as the 'Map' argument, and we guarantee
-- that the 'FMF's do not abut, that is, that if a tail of the result is @(off,
-- frag):(off':_):_@, then @off + lengthFMF frag < off'@.
flatten :: Map Word16 MemMapItem -> Slice16 -> [(Word16, FMF)]
flatten m s = go (start s) where
	go i
		| i > end s = []
		| otherwise = case MM.lookup i m of
			Empty memLoc -> next i memLoc
			Backed _ chunk chunkLoc memLoc -> cons i
				(foldMap (\s -> singleton (chunk, s))
				         (sizeFromEnd (min (end chunkLoc) (toInteger (end s))) chunkLoc)
				)
				(next i memLoc)
	next i slice =
		let i' = i + sizem116 slice + 1 in
		if i' <= i then [] else go i'

	cons i (FMF []) vs = vs
	cons i frag [] = [(i, frag)]
	cons i frag vs@(~(i', frag'):rest)
		| i + fromInteger (lengthFMF frag) == i' = (i, frag <> frag'):rest
		| otherwise = (i, frag):vs

lengthFMF :: FMF -> Integer
lengthFMF (FMF vs) = foldl' (\n (_, s) -> n + sizeU s) 0 vs

-- | Two fragments conflict if a non-empty strict prefix of one is a strict
-- suffix of the other. Note that a thing can conflict with itself!
conflicts :: FMF -> FMF -> Bool
conflicts (FMF vs) (FMF vs') = (go vs vs' || go vs' vs) where
	go vs vs' = any (`isFMFPrefixOf` vs') (strictTails vs)

	strictTails [] = [[]]
	strictTails ((chunk, slice):rest) = case sizeFromStart (start slice + 1) slice of
		Nothing -> tails rest
		Just slice' -> tails ((chunk, slice'):rest)

	isFMFPrefixOf ((chunk, slice):rest) ((chunk', slice'):rest')
		| chunk == chunk'
		&& start slice <= start slice'
		= case (rest, compare (end slice) (end slice')) of
		  	(_, EQ) -> rest `isStrictPrefixOf` rest'
		  	([], LT) -> True
		  	_ -> False
	-- VERY clever, refactor with care
	isFMFPrefixOf _ _ = False

	isStrictPrefixOf [] vs' = not (null vs')
	isStrictPrefixOf [(chunk, slice)] ((chunk', slice'):rest')
		= chunk == chunk'
		&& start slice == start slice'
		&& (  end slice < end slice'
		   || (end slice == end slice' && not (null rest'))
		   )
	isStrictPrefixOf ((chunk, slice):rest) [] = False
	isStrictPrefixOf (v:rest) (v':rest') = v == v' && isStrictPrefixOf rest rest'

-- | @splitPrefixFMF pre full = Just suf@ iff @pre <> suf = full@.
splitPrefixFMF :: FMF -> FMF -> Maybe FMF
splitPrefixFMF pre full = case sharedPrefix pre full of
	(_, FMF [], suf) -> Just suf
	_ -> Nothing

-- | If @sharedPrefix a b = (shared, asuf, bsuf)@, then @a = shared <> asuf@,
-- @b = shared <> bsuf@, and @asuf@ and @bsuf@ start with different @(ChunkID,
-- Word64)@ elements.
sharedPrefix :: FMF -> FMF -> (FMF, FMF, FMF)
sharedPrefix (FMF a) (FMF b) = (FMF shared, FMF asuf, FMF bsuf) where
	(shared, asuf, bsuf) = go a b
	go (v@(chunk, slice):rest) (v'@(chunk', slice'):rest')
		| v == v'
			= let ~(shared, suf, suf') = go rest rest' in (v:shared, suf, suf')
		| chunk == chunk' && start slice == start slice'
			= ( [(chunk, min slice slice')]
			  , maybe id (:) ((,) chunk <$> fromStartEnd start' (end slice )) rest
			  , maybe id (:) ((,) chunk <$> fromStartEnd start' (end slice')) rest'
			  )
			where
			start' = min (end slice) (end slice') + 1
	go a b = ([], a, b)

-- | Notionally, a @Map FMF a@. However, we maintain an invariant that all
-- 'FMF' keys are pairwise either nested or unambiguous. Nested means one is a
-- notional substring of the other. Unambiguous means no string has both as
-- overlapping substrings.
--
-- In particular, if @a@, @b@, and @c@ are non-empty, then @a <> b@ and @b <>
-- c@ can't both be keys, and @a <> d <> a@ can't be a key for any @d@.
data Folds a = Folds
	-- fragments: for a given chunk, a map from intervals in that chunk to all
	-- the 'FMF's that contain the given interval
	{ fragments :: Map ChunkID (IntervalMap SliceU (Set FMF)) -- poor man's suffix tree
	-- starts: for a given chunk, a map from offsets in that chunk to all the
	-- folds that start at the given offset
	, starts :: Map ChunkID (Map Integer (Map FMF a)) -- poor man's RLE trie
	} deriving (Eq, Ord, Show)
-- Invariant: No empty 'FMF's. Plus the invariants outlined in the field
-- descriptions above.

instance Default (Folds a) where def = Folds def def

-- | Create a new fold, initially closed. May fail if the given key 'conflicts'
-- with any existing key (or itself), in which case the set of conflicting keys
-- is returned. Overwrites any existing fold with the same key.
addFold :: FMF -> a -> Folds a -> Either (Set FMF) (Folds a)
addFold fragment a folds = case S.filter (conflicts fragment) candidates of
	s | S.size s > 0 -> Left s
	  | otherwise -> Right folds
	  	{ fragments = M.unionWith (IM.unionWith S.union)
	  		(fragmentsFromFragment fragment)
	  		(fragments folds)
	  	, starts = M.unionWith (M.unionWith M.union)
	  		(startsFromFragment fragment a)
	  		(starts folds)
	  	}
	where
	candidates = possibleConflicts fragment (fragments folds)

-- The only possible conflicts are existing keys that contain the first byte or
-- last byte of the fragment, or the new key itself.
possibleConflicts :: FMF -> Map ChunkID (IntervalMap SliceU (Set FMF)) -> Set FMF
possibleConflicts      (FMF []) frags = S.empty
possibleConflicts frag@(FMF vs) frags = S.unions ([S.singleton frag] ++ fmfs ++ fmfs') where
	(chunk , slice ):_ = vs
	(chunk', slice'):_ = reverse vs
	fmfs  = IM.elems $ IM.containing
		(M.findWithDefault IM.empty chunk  frags)
		(start slice)
	fmfs' = IM.elems $ IM.containing
		(M.findWithDefault IM.empty chunk' frags)
		(end slice')

fragmentsFromFragment :: FMF -> Map ChunkID (IntervalMap SliceU (Set FMF))
fragmentsFromFragment fragment@(FMF vs) = M.fromListWith (IM.unionWith S.union)
	[ (chunk, IM.singleton slice (S.singleton fragment))
	| (chunk, slice) <- vs
	]

startsFromFragment :: FMF -> a -> Map ChunkID (Map Integer (Map FMF a))
startsFromFragment (FMF []) a = M.empty
startsFromFragment fragment@(FMF ((chunk, slice):_)) a = id
	. M.singleton chunk
	. M.singleton (start slice)
	. M.singleton fragment
	$ a

-- | Delete a fold. The summary will be lost. Nested folds are retained.
removeFold :: FMF -> Folds a -> Folds a
removeFold fragment folds = folds
	{ fragments = M.unionWith (IM.unionWith (S.\\))
		(fragments folds)
		(fragmentsFromFragment fragment)
	, starts = M.unionWith (M.unionWith (M.\\))
		(starts folds)
		(startsFromFragment fragment (error "impossible: inspected sentinel value constructed in removeFold"))
	}

-- | Given a fragment of the current memory map, produce the tree of folds. The
-- returned 'Slice's are indices into the argument 'FMF'. 'Slice's at any given
-- level of the returned tree do not overlap.
view :: Folds a -> FMF -> Forest (SliceU, a)
view folds (FMF vs) = findFolds (starts folds) 0 vs where
	findFolds ss i [] = []
	findFolds ss i ((chunk, slice):rest) = case M.lookup chunk ss of
		Nothing -> findFolds ss (i+sizeU slice) rest
		Just m -> case M.splitLookup (start slice) m of
			(_, mfrags, fragss) -> findFoldsStarting ss i chunk slice rest
				(foldMap (\frags -> [(start slice, frags)]) mfrags ++ M.toAscList fragss)

	findFoldsStarting ss i chunk slice rest [] = findFolds ss (i+sizeU slice) rest
	findFoldsStarting ss i chunk slice rest ((start', frags):offfrags)
		| end slice < start' = findFolds ss (i+sizeU slice) rest
		| otherwise = case [ (frag, rest', a)
		                   | (frag@(FMF ((_, slice'):_)), a) <- M.toDescList frags
		                   , Just shortSlice <- [sizeFromStart (start slice') slice]
		                   , Just (FMF rest') <- [splitPrefixFMF frag (FMF ((chunk, shortSlice):rest))]
		                   ]
		              of
			(frag@(FMF vs'@((_, slice'):_)), rest', a):_ ->
				let sz = lengthFMF frag
				    ss' = ( flip M.adjust chunk
				          . flip M.adjust start'
				          $ M.delete frag
				          ) ss
				    i' = i+start slice'-start slice
				in Node (fromStartSizeBU i' sz, a) (findFolds ss' i' vs')
				:  findFolds ss (i'+sz) rest'
			_ -> findFoldsStarting ss i chunk slice rest offfrags
