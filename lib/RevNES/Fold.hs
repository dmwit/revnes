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
	( FMF, singleton, fromList, lengthFMF
	, Folds, addFold, removeFold, view
	) where

import Control.Monad
import Data.Foldable
import Data.List
import Data.IntervalMap (IntervalMap, Interval(..))
import Data.Map (Map)
import Data.Ord
import Data.Set (Set)
import Data.Tree
import RevNES.MemMap

import qualified Data.IntervalMap as IM
import qualified Data.Map as M
import qualified Data.Set as S

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
newtype FMF = FMF [(ChunkID, Slice)] deriving (Eq, Show)
-- Two invariants:
-- 1. No empty slices (all 'size's are > 0).
-- 2. Adjacent elements can't be collapsed, that is, they either have different
--    'ChunkID's or the end of the first one's slice is not one smaller than
--    the start of the second one's slice.

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
			-- overflow
			$ (if chunk == chunk' && offset slice + size slice == offset slice'
			   then [(chunk, slice { size = size slice + size slice' })]
			   else [v, v']
			  )
		go (v:vs) vs' = v:go vs vs'

singleton :: (ChunkID, Slice) -> FMF
singleton x@(chunk, slice) = FMF [x | size slice > 0]

fromList :: [(ChunkID, Slice)] -> FMF
fromList = foldMap singleton

-- overflow
lengthFMF :: FMF -> Word64
lengthFMF (FMF vs) = foldl' (+) 0 [size slice | (_, slice) <- vs]

-- | Two fragments conflict if a non-empty strict prefix of one is a strict
-- suffix of the other. Note that a thing can conflict with itself!
conflicts :: FMF -> FMF -> Bool
conflicts (FMF vs) (FMF vs') = (go vs vs' || go vs' vs) where
	go vs vs' = any (`isFMFPrefixOf` vs') (strictTails vs)

	strictTails [] = [[]]
	-- should never happen due to invariant (1), but...
	strictTails ((_, Slice { size = 0 }):rest) = strictTails rest
	strictTails ((_, Slice { size = 1 }):rest) = tails rest
	strictTails ((chunk, slice):rest) = tails ((chunk, slice'):rest) where
		-- overflow in the addition, maybe? the subtraction should be fine
		slice' = Slice { offset = offset slice + 1, size = size slice - 1 }

	isFMFPrefixOf ((chunk, slice):rest) ((chunk', slice'):rest')
		| chunk == chunk'
		&& offset slice <= offset slice'
		-- overflow
		= case (rest, compare (offset slice + size slice) (offset slice' + size slice')) of
		  	(_, EQ) -> rest `isStrictPrefixOf` rest'
		  	([], LT) -> True
		  	_ -> False
	-- VERY clever, refactor with care
	isFMFPrefixOf _ _ = False

	isStrictPrefixOf [] vs' = not (null vs')
	isStrictPrefixOf [(chunk, slice)] ((chunk', slice'):rest')
		= chunk == chunk'
		&& offset slice == offset slice'
		&& (  size slice < size slice'
		   -- this @not (null rest)@ check relies on invariant (1)
		   || (size slice == size slice' && not (null rest'))
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
		| chunk == chunk' && offset slice == offset slice'
			= ( [(chunk, slice { size = smallSize })]
			  , [(chunk, Slice newOffset newSize ) | newSize  /= 0] ++ rest
			  , [(chunk, Slice newOffset newSize') | newSize' /= 0] ++ rest'
			  )
			where
			smallSize = min (size slice) (size slice')
			-- overflow
			newOffset = offset slice + smallSize
			newSize  = size slice  - smallSize
			newSize' = size slice' - smallSize
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
	{ fragments :: Map ChunkID (IntervalMap Word64 (Set FMF)) -- poor man's suffix tree
	-- starts: for a given chunk, a map from offsets in that chunk to all the
	-- folds that start at the given offset
	, starts :: Map ChunkID (Map Word64 (Map FMF a)) -- poor man's RLE trie
	} deriving (Eq, Ord, Show)
-- Invariant: No empty 'FMF's. Plus the invariants outlined in the field
-- descriptions above.

empty :: Folds summary
empty = Folds M.empty M.empty

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
possibleConflicts :: FMF -> Map ChunkID (IntervalMap Word64 (Set FMF)) -> Set FMF
possibleConflicts      (FMF []) frags = S.empty
possibleConflicts frag@(FMF vs) frags = S.unions ([S.singleton frag] ++ fmfs ++ fmfs') where
	(chunk , slice ):_ = vs
	(chunk', slice'):_ = reverse vs
	fmfs  = IM.elems $ IM.containing
		(M.findWithDefault IM.empty chunk  frags)
		(offset slice)
	fmfs' = IM.elems $ IM.containing
		(M.findWithDefault IM.empty chunk' frags)
		-- overflow
		(offset slice' + size slice' - 1)

fragmentsFromFragment :: FMF -> Map ChunkID (IntervalMap Word64 (Set FMF))
fragmentsFromFragment fragment@(FMF vs) = M.fromListWith (IM.unionWith S.union)
	-- overflow
	[ (chunk, IM.singleton (IntervalCO o (o+s)) (S.singleton fragment))
	| (chunk, Slice { offset = o, size = s }) <- vs
	]

startsFromFragment :: FMF -> a -> Map ChunkID (Map Word64 (Map FMF a))
startsFromFragment (FMF []) a = M.empty
startsFromFragment fragment@(FMF ((chunk, slice):_)) a = id
	. M.singleton chunk
	. M.singleton (offset slice)
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
view :: Folds a -> FMF -> Forest (Slice, a)
view folds (FMF vs) = findFolds (starts folds) 0 vs where
	-- overflow many places in this where block
	findFolds ss i [] = []
	findFolds ss i ((chunk, slice):rest) = case M.lookup chunk ss of
		Nothing -> findFolds ss (i+size slice) rest
		Just m -> case M.splitLookup (offset slice) m of
			(_, mfrags, fragss) -> findFoldsStarting ss i chunk slice rest
				(foldMap (\frags -> [(offset slice, frags)]) mfrags ++ M.toAscList fragss)

	findFoldsStarting ss i chunk slice rest [] = findFolds ss (i+size slice) rest
	findFoldsStarting ss i chunk slice rest ((offset', frags):offfrags)
		| offset slice + size slice < offset' = findFolds ss (i+size slice) rest
		| otherwise = case [ (frag, rest', a)
		                   | (frag@(FMF ((_, slice'):_)), a) <- M.toDescList frags
		                   , Just (FMF rest') <- [splitPrefixFMF frag (FMF ((chunk, slice' { size = offset slice + size slice - offset slice' }):rest))]
		                   ]
		              of
			(frag@(FMF vs@((_, slice'):_)), rest', a):_ ->
				let sz = lengthFMF frag
				    ss' = ( flip M.adjust chunk
				          . flip M.adjust offset'
				          $ M.delete frag
				          ) ss
				    i' = i+offset slice'-offset slice
				in Node (Slice i' sz, a) (findFolds ss' i' vs)
				:  findFolds ss (i'+sz) rest'
			_ -> findFoldsStarting ss i chunk slice rest offfrags
