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
import qualified Data.Map as M

data Slice = Slice { offset, size :: Word64 } deriving (Eq, Ord, Read, Show)

-- | A source of bytes. The most basic is a slice of a file. We also support
-- looking inside a zip archive for a 'Source'.
data Source
	= File FilePath Slice
	| Zip FilePath Source
	deriving (Eq, Ord, Read, Show)

data MemMapItem
	= Bytes ByteString (Digest SHA256) Slice -- contents, source, location in the context item
	| Mirror Slice
	deriving (Eq, Ord, Show)

mmiSlice :: MemMapItem -> Slice
mmiSlice (Bytes _ _ s) = s
mmiSlice (Mirror s) = s

data MemMap = MemMap { cpu, ppu :: Map Word16 MemMapItem } deriving (Eq, Ord, Show)
instance Default MemMap where def = MemMap def def

type MemMaps = Map String MemMap

data Chunk = Chunk
	{ source :: Source
	, content :: ByteString
	, description :: String
	} deriving (Eq, Ord, Show)

type Chunks = Map (Digest SHA256) Chunk
