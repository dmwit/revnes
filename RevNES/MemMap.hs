module RevNES.MemMap where

import Crypto.Hash (Digest, SHA256)
import Data.ByteString (ByteString)
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
	= Bytes ByteString (Digest SHA256) Word64 -- contents, source, offset in the context item
	| Mirror Slice
	deriving (Eq, Ord, Show)

data MemMap = MemMap { cpu, ppu :: Map Word16 MemMapItem } deriving (Eq, Ord, Show)
type MemMaps = Map String MemMap

data ContextItem = ContextItem
	{ source :: Source
	, content :: ByteString
	, description :: String
	} deriving (Eq, Ord, Show)

type Context = Map (Digest SHA256) ContextItem
