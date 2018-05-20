{-# LANGUAGE BinaryLiterals #-}
module RevNES.MemMap.INES where

import Codec.Archive.Zip
import Control.Applicative
import Control.Exception
import Control.Monad.Except
import Crypto.Hash (hash)
import Data.Attoparsec.ByteString.Lazy (Parser, Result(Done,Fail), anyWord8, endOfInput, parse, satisfy, string)
import Data.Bifunctor
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable
import Data.List
import Data.Map (Map)
import Data.Word
import RevNES.MemMap
import System.FilePath
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M

autodetectLoad :: FilePath -> ExceptT String IO (Mapper, Context, MemMap)
autodetectLoad fp = case takeExtension fp of
	".zip" -> loadZip fp
	_ -> tryReadFile fp >>= loadByteString (File fp)

loadZip :: FilePath -> ExceptT String IO (Mapper, Context, MemMap)
loadZip fp = do
	bs <- tryReadFile fp
	archive <- either throwError return $ toArchiveOrFail bs
	entry <- case [e | e <- zEntries archive, takeExtension (eRelativePath e) == ".nes"] of
		[] -> throwError (fp ++ " contains no files named *.nes")
		[entry] -> return entry
		entries -> throwError (fp ++ " contains multiple files named *.nes: " ++ show entries)
	loadByteString (Zip fp . File (eRelativePath entry)) (fromEntry entry)

loadByteString :: (Slice -> Source) -> LBS.ByteString -> ExceptT String IO (Mapper, Context, MemMap)
loadByteString src bs = case parse parseINES bs of
	Done rest ines
		| rest == LBS.empty -> inesToMemMap src ines
		| otherwise -> throwError
			$  "The impossible happened! Parse of " ++ ppSource src ++ "\n\t"
			++ "succeeded without consuming the entire file contents.\n\t"
			++ "(Please report this as a bug.)"
	Fail rest ctxts err -> throwError
		$  "Invalid NES rom in " ++ ppSource src ++ "\n\t"
		++ intercalate "," ctxts ++ "\n\t"
		++ err

inesToMemMap :: (Slice -> Source) -> INES -> ExceptT String IO (Mapper, Context, MemMap)
inesToMemMap src ines = do
	context <- mContext
	(prgA, prgB) <- case prgs ines of
		[] -> throwError $ "There are no PRG ROMs in " ++ inesName ++ " to put in the memory map."
		[prg] | BS.length prg == 0x4000 -> return (prg, prg)
		prgA:prgB:_
			|  BS.length prgA <= 0x4000
			&& BS.length prgB == 0x4000 -> return (prgA, prgB)
		_ -> throwError $ "The impossible happened! The PRG ROMs in " ++ inesName ++ " are not exactly 16KiB long."
	return (mapper (header ines), context, buildMemMap prgA prgB)
	where
	mContext = id
		. traverse validateContextItems
		. M.fromListWith (++)
		. map (\((bs, name), offset) -> (hash bs, [ContextItem
		  	{ source = src (Slice offset (fromIntegral (BS.length bs)))
		  	, content = bs
		  	, description = name
		  	}]))
		. zip chunks
		. scanl (\offset (bs, _) -> offset + fromIntegral (BS.length bs)) headerLen
		$ chunks

	validateContextItems :: [ContextItem] -> ExceptT String IO ContextItem
	validateContextItems [] = throwError "The impossible happened! Got an empty list of context items when trying to validate them in inesToMemMap."
	validateContextItems (i:is) = do
		case filter (\i' -> content i /= content i') is of
			[] -> return ()
			i':_ -> throwError
				$  "Wow, found a hash collision in " ++ inesName ++ ":\n\t"
				++ "bytes " ++ ppSlice (source i ) ++ " and \n\t"
				++ "bytes " ++ ppSlice (source i')
		return $ case reverse is of
			[]     -> i { description = description i ++ " for " ++ inesName }
			[i']   -> i { description = description i ++ " and " ++ description i' ++ " for " ++ inesName }
			i':is' -> i { description
				=  intercalate ", " (map description (i:reverse is'))
				++ ", and " ++ description i'
				++ " for " ++ inesName
				}

	chunks = [(bs, "trainer") | bs <- toList (trainer ines)]
	      ++ [(bs, "PRG " ++ show n) | (n, bs) <- zip [0..] (prgs ines)]
	      ++ [(bs, "CHR " ++ show n) | (n, bs) <- zip [0..] (chrs ines)]
	      -- playChoice stuff isn't mapped anywhere for now, no idea where it should go

	buildMemMap prgA prgB = MemMap
		{ cpu = M.fromList
			$  [ (0x8000, Bytes prgA (hash prgA) 0)
			   , (0xc000, Bytes prgB (hash prgB) 0)
			   ]
			++ cpuMirrors
		, ppu = M.fromList (ppuMappings ++ ppuMirrors)
		}

	ppuMappings = zip [0x0000]
		[Bytes bs (hash bs) 0 | bs <- chrs ines, BS.length bs == 0x2000]

	ppuMirrors = case mirroring (header ines) of
		Horizontal -> [(0x2400, Mirror (Slice 0x2000 0x0400)), (0x2c00, Mirror (Slice 0x2800 0x0400))]
		Vertical   -> [(0x2800, Mirror (Slice 0x2000 0x0400)), (0x2c00, Mirror (Slice 0x2400 0x0400))]
		FourScreen -> []

	cpuMirrors =
		[ (0x0800, Mirror (Slice 0x0000 0x0800))
		, (0x1000, Mirror (Slice 0x0000 0x1000))
		, (0x2008, Mirror (Slice 0x2000 0x0008))
		, (0x2010, Mirror (Slice 0x2000 0x0010))
		, (0x2020, Mirror (Slice 0x2000 0x0020))
		, (0x2040, Mirror (Slice 0x2000 0x0040))
		, (0x2080, Mirror (Slice 0x2000 0x0080))
		, (0x2100, Mirror (Slice 0x2000 0x0100))
		, (0x2200, Mirror (Slice 0x2000 0x0200))
		, (0x2400, Mirror (Slice 0x2000 0x0400))
		, (0x2800, Mirror (Slice 0x2000 0x0800))
		]

	headerLen = 0x10
	srcName = ppSource src
	inesName = case BS.filter (>0) (name ines) of
		bs | bs == BS.empty -> srcName
		   | otherwise      -> BS8.unpack bs ++ " (" ++ srcName ++ ")"

ppSlice :: Source -> String
ppSlice (File fp (Slice o s)) = show o ++ "-" ++ show (o+s)
ppSlice (Zip _ s) = ppSlice s

ppSource :: (Slice -> Source) -> String
ppSource src = go (src (Slice 0 0)) where
	go (File fp _) = fp
	go (Zip fp s) = fp ++ ":" ++ go s

tryReadFile :: FilePath -> ExceptT String IO LBS.ByteString
tryReadFile = ExceptT . fmap (first (show :: IOException -> String)) . try . LBS.readFile

data Mirroring = Horizontal | Vertical | FourScreen
	deriving (Bounded, Enum, Eq, Ord, Read, Show)
data TVSystem = NTSC | PAL | DualCompatible
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Counts are in multiples of 8KiB.
data BatteryBackedRAM
	= None
	| OfficialOnly Word8
	| UnofficialOnly Word8
	| OfficialAndUnofficial Word8
	deriving (Eq, Ord, Read, Show)

-- | If it is declared to have battery backed RAM in either the official or the
-- unofficial flag, return how much RAM it's declared to have.
batteryBackedRAMSize :: BatteryBackedRAM -> Maybe Word8
batteryBackedRAMSize bb = case bb of
	None -> Nothing
	OfficialOnly          w -> Just w
	UnofficialOnly        w -> Just w
	OfficialAndUnofficial w -> Just w

data PreParseHeader = PreParseHeader
	{ pp_prgROMSize :: Word8 -- in multiples of 16KiB
	, pp_chrROMSize :: Word8 -- in multiples of  8KiB
	, pp_mirroring :: Mirroring
	, pp_hasTrainer :: Bool
	, pp_mapper :: Word8
	, pp_vsUnisystem :: Bool
	, pp_playChoice10 :: Bool
	, pp_batteryBackedRAM :: BatteryBackedRAM
	, pp_system :: TVSystem
	, pp_system2 :: TVSystem          -- from unofficial flag
	, pp_hasBusConflicts :: Bool      -- from unofficial flag
	}
	deriving (Eq, Ord, Read, Show)

data Header = Header
	{ mirroring :: Mirroring
	, mapper :: Word8
	, vsUnisystem :: Bool
	, batteryBackedRAM :: BatteryBackedRAM
	, system :: TVSystem
	, system2 :: TVSystem          -- from unofficial flag
	, hasBusConflicts :: Bool      -- from unofficial flag
	}
	deriving (Eq, Ord, Read, Show)

preParseHeaderToHeader :: PreParseHeader -> Header
preParseHeaderToHeader pph = Header
	{ mirroring = pp_mirroring pph
	, mapper = pp_mapper pph
	, vsUnisystem = pp_vsUnisystem pph
	, batteryBackedRAM = pp_batteryBackedRAM pph
	, system = pp_system pph
	, system2 = pp_system2 pph
	, hasBusConflicts = pp_hasBusConflicts pph
	}

data PlayChoice
	= PCROMOnly ByteString
	| PCROMAndKeys ByteString ByteString ByteString
	deriving (Eq, Ord, Read, Show)

type Mapper = Word8

data INES = INES
	{ header :: Header
	, trainer :: Maybe ByteString
	, prgs :: [ByteString] -- each 16KiB
	, chrs :: [ByteString] -- each  8KiB
	, playChoice :: Maybe PlayChoice
	, name :: ByteString
	} deriving (Eq, Ord, Read, Show)

parseHeader :: Parser PreParseHeader
parseHeader = do
	string (BS.pack [0x4e, 0x45, 0x53, 0x1a])
	prgRoms <- anyWord8
	chrRoms <- anyWord8
	f6 <- anyWord8
	f7 <- anyWord8
	when (f7 .&. 0b00001100 == 0b00001000)
	     (fail "NES 2.0 format is currently unsupported")
	prgRams <- max 1 <$> anyWord8
	tvSystem <- satisfy (<2)
	f10 <- anyWord8
	string (BS.replicate 5 0)
	return PreParseHeader
		{ pp_prgROMSize = prgRoms
		, pp_chrROMSize = chrRoms
		, pp_mirroring = case (testBit f6 0, testBit f6 3) of
		  	(_, True)  -> FourScreen
		  	(False, _) -> Horizontal
		  	(True , _) -> Vertical
		, pp_hasTrainer = testBit f6 2
		, pp_mapper = shiftR (f6 .&. 0xf0) 4 .|. (f7 .&. 0xf0)
		, pp_vsUnisystem = testBit f7 0
		, pp_playChoice10 = testBit f7 1
		, pp_batteryBackedRAM = case (testBit f6 1, testBit f10 4) of
		  	(False, False) -> None
		  	(False, True ) -> UnofficialOnly prgRams
		  	(True , False) -> OfficialOnly prgRams
		  	(True , True ) -> OfficialAndUnofficial prgRams
		, pp_system = if tvSystem == 0 then NTSC else PAL
		, pp_system2 = case f10 .&. 0x03 of
		  	0 -> NTSC
		  	2 -> PAL
		  	_ -> DualCompatible
		, pp_hasBusConflicts = testBit f10 5
		}

parsePlayChoice :: Parser PlayChoice
parsePlayChoice = P.take 0x2000 >>= \rom ->
	(do key1 <- P.take 0x10
	    key2 <- P.take 0x10
	    return (PCROMAndKeys rom key1 key2)
	) <|> return (PCROMOnly rom)

parseINES :: Parser INES
parseINES = do
	pph <- parseHeader
	trn <- if pp_hasTrainer pph then Just <$> P.take 0x200 else return Nothing
	prgROMs <- replicateM (fromIntegral (pp_prgROMSize pph)) (P.take 0x4000)
	chrROMs <- replicateM (fromIntegral (pp_chrROMSize pph)) (P.take 0x2000)
	-- TODO: This does not gracefully handle the situation where there is a
	-- PlayChoice ROM only (no keys) and a name, because parsePlayChoice will
	-- take the first 0x20 bytes of the name and treat it as PlayChoice keys.
	pc <- if pp_playChoice10 pph then Just <$> parsePlayChoice else return Nothing
	n <- asum [P.take 128, P.take 127, return BS.empty]
	endOfInput
	return INES
		{ header = preParseHeaderToHeader pph
		, trainer = trn
		, prgs = prgROMs
		, chrs = chrROMs
		, playChoice = pc
		, name = n
		}
