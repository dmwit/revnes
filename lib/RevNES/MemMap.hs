module RevNES.MemMap
	( module RevNES.MemMap
	, Word16
	, Word64
	, Digest
	, SHA256
	) where

import Crypto.Hash (Digest, SHA256)
import Data.ByteString (ByteString)
import Data.Default
import Data.Map (Map)
import Data.Word
import Prelude hiding (lookup)
import RevNES.Slice
import qualified Data.ByteString as BS
import qualified Data.Map as M

-- | A source of bytes. The most basic is a slice of a file. We also support
-- looking inside a zip archive for a 'Source'.
data Source
	= File FilePath SliceU
	| Zip FilePath Source
	deriving (Eq, Ord, Read, Show)

data MemMapItem
	= Bytes ByteString ChunkID SliceU -- contents, source, location in the context item
	| Mirror Slice16
	deriving (Eq, Ord, Show)

mmiSlice :: MemMapItem -> SliceU
mmiSlice (Bytes _ _ s) = s
mmiSlice (Mirror s) = u s

data MemMap = MemMap { cpu, ppu :: Map Word16 MemMapItem } deriving (Eq, Ord, Show)
instance Default MemMap where def = MemMap def def

data MemMapResult
	= Backed
		{ bytes :: ByteString
		, chunk :: ChunkID
		, chunkLoc :: SliceU
		, memLoc :: Slice16
		}
	| Empty
		{ memLoc :: Slice16
		}
	deriving (Eq, Ord, Show)

-- | Look up an address, resolving any mirroring. The 'MemMapResult' returned
-- may give results for following addresses that get resolved "in the same
-- way". No promise that the result is the maximal one with this property.
lookup :: Word16 -> Map Word16 MemMapItem -> MemMapResult
lookup addr memMap = case M.lookupLE addr memMap of
	Just (startAddr, Mirror slice)
		| Just i <- index addr (endFromStartB startAddr slice) ->
			let res = lookup (start slice + i) memMap
			    sizem1' = min (sizem116 (memLoc res)) (sizem116 slice - i)
			in res { memLoc = endFromSizem1B sizem1' (memLoc res) }
	Just (startAddr, Bytes bs ch slice)
		| Just i <- index addr memSlice -> Backed
			{ bytes = BS.drop (fromIntegral i) bs
			, chunk = ch
			, chunkLoc = sizeFromStartB (start slice + fromIntegral i) slice
			, memLoc = sizeFromStartB addr memSlice
			}
		where memSlice = bB (endFromStartU (toInteger startAddr) slice)
	_ -> Empty . fromStartSizeB addr $ case M.lookupGT addr memMap of
		Just (endAddr, _) -> endAddr - addr
		Nothing -> maxBound

type MemMaps = Map String MemMap

type ChunkID = Digest SHA256
data Chunk = Chunk
	{ source :: Source
	, content :: ByteString
	, description :: String
	} deriving (Eq, Ord, Show)

type Chunks = Map ChunkID Chunk
