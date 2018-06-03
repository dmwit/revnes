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
import qualified Data.ByteString as BS
import qualified Data.Map as M

data Slice = Slice { offset, size :: Word64 } deriving (Eq, Ord, Read, Show)

-- | A source of bytes. The most basic is a slice of a file. We also support
-- looking inside a zip archive for a 'Source'.
data Source
	= File FilePath Slice
	| Zip FilePath Source
	deriving (Eq, Ord, Read, Show)

data MemMapItem
	= Bytes ByteString ChunkID Slice -- contents, source, location in the context item
	| Mirror Slice
	deriving (Eq, Ord, Show)

mmiSlice :: MemMapItem -> Slice
mmiSlice (Bytes _ _ s) = s
mmiSlice (Mirror s) = s

data MemMap = MemMap { cpu, ppu :: Map Word16 MemMapItem } deriving (Eq, Ord, Show)
instance Default MemMap where def = MemMap def def

data MemMapResult
	= Backed
		{ bytes :: ByteString
		, chunk :: ChunkID
		, chunkLoc :: Slice
		, memLoc :: Slice
		}
	| Empty
		{ memLoc :: Slice
		}
	deriving (Eq, Ord, Show)

-- | Look up an address, resolving any mirroring. The 'MemMapResult' returned
-- may give results for following addresses that get resolved "in the same
-- way". No promise that the result is the maximal one with this property, but
-- it definitely won't be a zero-length slice.
lookup :: Word16 -> Map Word16 MemMapItem -> MemMapResult
lookup addr memMap = case M.lookupLE addr memMap of
	Just (startAddr, Mirror Slice { offset = addr', size = sz })
		| fromIntegral (addr - startAddr) < sz ->
			let res = lookup (fromIntegral addr' + addr - startAddr) memMap
			    memLoc' = (memLoc res) { size = min (size (memLoc res)) (after sz startAddr addr) }
			in res { memLoc = memLoc' }
	Just (startAddr, Bytes bs ch Slice { offset = addr', size = sz })
		| fromIntegral (addr - startAddr) < sz -> Backed
			{ bytes = BS.drop (fromIntegral (addr - startAddr)) bs
			, chunk = ch
			-- overflow in first addition only
			, chunkLoc = Slice (addr' + fromIntegral (addr - startAddr)) (after sz startAddr addr)
			, memLoc = Slice (fromIntegral addr) (after sz startAddr addr)
			}
	_ -> Empty . Slice (fromIntegral addr) $ case M.lookupGT addr memMap of
		Just (endAddr, _) -> fromIntegral (endAddr - addr)
		Nothing -> 2^16 - fromIntegral addr
	where
	after sz startAddr addr = sz + fromIntegral startAddr - fromIntegral addr

type MemMaps = Map String MemMap

type ChunkID = Digest SHA256
data Chunk = Chunk
	{ source :: Source
	, content :: ByteString
	, description :: String
	} deriving (Eq, Ord, Show)

type Chunks = Map ChunkID Chunk
