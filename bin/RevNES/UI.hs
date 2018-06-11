module RevNES.UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.List
import Data.ByteString (ByteString)
import Data.Char
import Data.Default
import Data.IntMap (IntMap)
import Data.Maybe
import Data.Map (Map)
import Data.Monoid
import Data.String
import Data.Tree (Forest, Tree(..))
import Data.Text.Zipper
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro.Mtl (view)
import Numeric
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Vector as V

import RevNES.Fold (FMF, Folds)
import RevNES.MemMap
import RevNES.PrettyPrint.Pseudo (pp)
import RevNES.Types
import qualified RevNES.Fold as F
import qualified RevNES.MemMap as MM

data Focus = CommandLine | MessageLog
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Default Focus where def = CommandLine

data Tab = CPU | Maps
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Default Tab where def = CPU

data Severity = Success | Info | Warning | Error
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

severityAttr :: Severity -> AttrName
severityAttr s = fromString "severity" <> (fromString . map toLower . show) s

data Message = Message { severity :: Severity, message :: String }
	deriving (Eq, Ord, Read, Show)

renderMessage :: Message -> Widget n
renderMessage m = withAttr
	(severityAttr (severity m))
	(str (message m))

data FoldItem a = FoldItem
	{ open :: Bool
	, item :: a
	} deriving (Eq, Ord, Read, Show)

data FoldContent = User String | Instruction Instruction
	deriving (Eq, Ord, Read, Show)

data BytesRepresentation = Raw ByteString | Folded FoldContent
	deriving (Eq, Ord, Read, Show)

data ByteRegion = ByteRegion
	{ regionStart :: Word16
	, regionEnd :: Word16
	, regionDepth :: Int
	, regionContent :: BytesRepresentation
	} deriving (Eq, Ord, Read, Show)

type ByteRegions = Map Word16 ByteRegion

byteRegionZipper :: Word16 -> ByteRegions -> ([ByteRegion], Maybe ByteRegion, [ByteRegion])
byteRegionZipper w brs = case splitLookupZipper w brs of
	(lt@(ByteRegion { regionContent = Raw bs }):lts, Nothing, gts) | w <= regionEnd lt
		-> let (b, e) = BS.splitAt (fromIntegral (w - regionStart lt)) bs
		in ( lt { regionEnd = w-1, regionContent = Raw b }:lts
		   , Just lt { regionStart = w, regionContent = Raw e }
		   , gts)
	(lt:lts, Nothing, gts) | w <= regionEnd lt -> (lts, Just lt, gts)
	res -> res
	where
	splitLookupZipper a b = let (lt, eq, gt) = M.splitLookup a b
	                        in (snd <$> M.toDescList lt, eq, snd <$> M.toAscList gt)

data ByteRegionsDisplay = ByteRegionsDisplay
	{ regions :: ByteRegions
	, topRegionStart :: Word16
	, hiddenLines :: Int
	} deriving (Eq, Ord, Read, Show)

instance Default ByteRegionsDisplay where
	def = ByteRegionsDisplay
		{ regions = def
		, topRegionStart = 0
		, hiddenLines = 0
		}

data UI = UI
	{ focus :: Focus
	, tab :: Tab
	, commandLine :: Editor String Focus
	, messageLog :: List Focus Message
	, memMapSources :: Chunks
	, memMaps :: MemMaps
	, selectedMemMapName, visibleMemMapName :: String
	, nextFoldID :: Int
	, foldContents :: IntMap (FoldItem FoldContent)
	, folds :: Folds Int
	-- the ByteRegions contained in cpuDisplay is a cache of calling byteRegions
	, cpuDisplay :: ByteRegionsDisplay
	} deriving Show

instance Default UI where
	def = UI
		{ focus = def
		, tab = def
		, commandLine = editor CommandLine (Just 1) ""
		, messageLog = list MessageLog V.empty 1
		, memMapSources = def
		, memMaps = def
		, selectedMemMapName = "default"
		, visibleMemMapName = "default"
		, nextFoldID = minBound
		, foldContents = def
		, folds = def
		, cpuDisplay = def
		}

selectedMemMap :: UI -> MemMap
selectedMemMap ui = M.findWithDefault def (selectedMemMapName ui) (memMaps ui)

visibleMemMap :: UI -> MemMap
visibleMemMap ui = M.findWithDefault def (visibleMemMapName ui) (memMaps ui)

byteRegions :: UI -> ByteRegions
byteRegions ui = M.fromAscList
	[ (regionStart br, br)
	| (addr, frag) <- F.flatten memMap (Slice 0 (2^16))
	, br <- byteRegionsForView addr 0 (Slice 0 (F.lengthFMF frag)) (F.view (folds ui) frag)
	]
	where
	memMap = cpu (selectedMemMap ui)
	-- addr never changes in recursive calls. other args do
	byteRegionsForView addr depth Slice{size = 0} _ = []
	byteRegionsForView addr depth slice [] =
		let resolved = MM.lookup (addr + fromIntegral (offset slice)) memMap
		    s = min (size slice) (size (memLoc resolved))
		    recurse = byteRegionsForView addr depth (Slice (offset slice + s) (size slice - s)) []
		in case resolved of
		Empty{} -> recurse
		Backed { bytes = bs } -> ByteRegion
			{ regionStart = addr + fromIntegral (offset slice)
			, regionEnd = addr + fromIntegral (offset slice + s - 1)
			, regionDepth = depth
			, regionContent = Raw (BS.take (fromIntegral s) bs)
			} : recurse
	byteRegionsForView addr depth slice ns@(Node (foldSlice, foldId) children:forest)
		| offset slice < offset foldSlice
			=  byteRegionsForView addr depth slice{ size = offset foldSlice - offset slice } []
			++ byteRegionsForView addr depth (Slice (offset foldSlice) (offset slice + size slice - offset foldSlice)) ns
		| open foldItem
			=  byteRegionsForView addr (depth+1) foldSlice children
			++ byteRegionsForView addr depth (Slice (offset foldSlice + size foldSlice) (size slice - size foldSlice)) forest
		| otherwise = ByteRegion
			{ regionStart = addr + fromIntegral (offset foldSlice)
			, regionEnd = addr + fromIntegral (offset foldSlice + size foldSlice - 1)
			, regionDepth = depth
			, regionContent = Folded (item foldItem)
			} : byteRegionsForView addr depth (Slice (offset foldSlice + size foldSlice) (size slice - size foldSlice)) forest
		where foldItem = foldContents ui IM.! foldId

addMessage :: Severity -> String -> List n Message -> List n Message
addMessage s c = listMoveTo 0 . listInsert 0 (Message s c)

app :: App UI () Focus
app = App
	{ appDraw = (:[]) . draw
	, appChooseCursor = const listToMaybe
	, appHandleEvent = handleEvent
	, appStartEvent = return
	, appAttrMap = const style
	} where

	draw ui = joinBorders . borderWithLabel (title "Command line")
		 $            renderEditor (vBox . map str)    (focus ui == CommandLine) (commandLine ui)
		<=> hBorderWithLabel (title "Messages")
		<=> vLimit 3 (renderList (const renderMessage) (focus ui == MessageLog ) (messageLog  ui))
		<=> renderTab ui

	handleEvent ui e = case e of
		VtyEvent (EvKey (KChar '1') [MMeta]) -> continue (ui { tab = CPU })
		VtyEvent (EvKey (KChar '2') [MMeta]) -> continue (ui { tab = Maps  })
		VtyEvent (EvKey (KChar '1') [MCtrl]) -> continue (ui { focus = CommandLine })
		VtyEvent (EvKey (KChar '2') [MCtrl]) -> continue (ui { focus = MessageLog  })
		VtyEvent (EvKey (KChar 'c') [MCtrl]) -> halt ui
		VtyEvent e -> case (focus ui, e) of
			(CommandLine, EvKey KEnter []) -> do
				let cl = commandLine ui
				    ml = addMessage Info (head (getEditContents cl)) (messageLog ui)
				    ui' = handleCommand ui (head (getEditContents cl))
				continue (ui' { commandLine = applyEdit clearZipper cl })
			(CommandLine, _) -> do
				cl <- handleEditorEvent e (commandLine ui)
				continue ui { commandLine = cl }
			(MessageLog , _) -> do
				ml <- handleListEventVi handleListEvent e (messageLog ui)
				continue ui { messageLog = ml }
		_ -> continue ui

	style = attrMap defAttr
		[ (listSelectedFocusedAttr, currentAttr `withStyle` bold)
		, (severityAttr Success, fg green)
		, (severityAttr Warning, fg magenta `withStyle` bold)
		, (severityAttr Error  , fg red `withStyle` bold)
		, (emptyAttr, fg brightBlue)
		, (bugAttr, fg black <> bg red)
		, (openFoldAttr, fg yellow `withStyle` bold)
		, (labelAttr, fg green)
		]

handleCommand :: UI -> String -> UI
handleCommand ui s = case words s of
	[] -> ui
	["goto", addrString] -> case readAddr addrString of
		Just addr -> ui { cpuDisplay = (cpuDisplay ui) { topRegionStart = addr } }
		_ -> noParse
	"fold":startAddrString:endAddrString:summary -> case mapM readAddr [startAddrString, endAddrString] of
		Just [startAddr, endAddr] | endAddr > startAddr -> case F.flatten (cpu (selectedMemMap ui)) (Slice (fromIntegral startAddr) (fromIntegral (endAddr - startAddr + 1))) of
			[(addr, fmf)] | addr == startAddr && F.lengthFMF fmf == fromIntegral (endAddr - startAddr + 1)
				-> case F.addFold fmf (nextFoldID ui) (folds ui) of
					-- TODO: this error could use some beautification
					Left conflicts -> complain $ printf "Folding %04x-%04x would conflict with existing fold(s): %s" startAddr endAddr (show conflicts)
					Right folds' -> let
						ui' = ui
							{ nextFoldID = nextFoldID ui + 1
							, foldContents = IM.insert (nextFoldID ui) (FoldItem False (User (unwords summary))) (foldContents ui)
							, folds = folds'
							, cpuDisplay = (cpuDisplay ui) { regions = byteRegions ui' }
							, messageLog = addMessage Success (printf "Created fold around %04x-%04x" startAddr endAddr) (messageLog ui)
							}
						in ui'
			_ -> complain $ printf "Address range %04x-%04x includes unmapped memory" startAddr endAddr
		_ -> noParse
	_ -> noParse
	where
	-- TODO: actual error reporting. bonus points for actual parsing.
	noParse :: UI
	noParse = complain ("Could not parse command: " ++ s)

	complain :: String -> UI
	complain err = ui { messageLog = addMessage Error err (messageLog ui) }

	readAddr :: String -> Maybe Word16
	readAddr s = case readHex s of
		[(addr, "")] | 0 <= addr && addr < 2^16 -> Just (fromInteger addr)
		_ -> Nothing

renderTab :: UI -> Widget n
renderTab ui = case tab ui of
	CPU -> renderCPUMemory ui
	Maps -> renderMemMapInfo ui

renderCPUMemory :: UI -> Widget n
renderCPUMemory ui
	=   title "CPU memory"
	<=> renderByteRegions (cpuDisplay ui)

renderByteRegions :: ByteRegionsDisplay -> Widget n
renderByteRegions brd = Widget Fixed Greedy $ do
	h <- view availHeightL
	case byteRegionZipper (topRegionStart brd) (regions brd) of
		(_, Nothing, brs) -> render . vBox $ go 0 h brs
		(_, Just br, brs) -> render . vBox $ go (hiddenLines brd) h (br:brs)
	where
	go _ 0 _ = []
	go _ _ [] = []
	go hide height (br:brs) = case regionContent br of
		-- TODO: not def
		Folded (Instruction i) -> recurse (pp def (regionStart br) i)
		Folded (User s) -> recurse s
		Raw bs -> [ bugStr "tried hiding some rows of a single, raw, unfolded byte" | hide /= 0]
		       ++ [ hBox
		          	[ str . showDepth . regionDepth $ br
		          	, withAttr labelAttr . str $ printf "      %04x " addr
		          	, withAttr openFoldAttr . str $ printf "0x%02x" byte
		          	]
		          | (addr, byte) <- zip [regionStart br..] $ BS.unpack (BS.take height bs)
		          ]
		       ++ go 0 (height-BS.length bs) brs
		where
		recurse s = let sLines = lines s; len = length sLines in hBox
			[ str . showDepth . regionDepth $ br
			, str $ case (hide > 0, len - hide > height, height < 2) of
				(False, False, _    ) -> " "
				(False, True , _    ) -> replicate (height-1) '\n' ++ "▼"
				(True , False, _    ) -> "▲"
				(True , True , False) -> "▲" ++ replicate (height-2) '\n' ++ "▼"
				(True , True , True ) -> "♦"
			, if regionStart br < regionEnd br
			  then renderLabel (regionStart br) <+> str "-"
			  else str "     "
			, renderLabel (regionEnd br)
			, str " "
			, withAttr closedFoldAttr . str . unlines . take height . drop hide $ sLines
			] : go 0 (height+hide-len) brs

	showDepth n | n < 1 = " "
	            | n < 10 = show n
	            | otherwise = "+"

closedFoldAttr :: AttrName
closedFoldAttr = fromString "closed fold"

openFoldAttr :: AttrName
openFoldAttr = fromString "open fold"

bugAttr :: AttrName
bugAttr = fromString "buggy behavior that users should never see"

bugStr :: String -> Widget n
bugStr = withAttr bugAttr . str . ("bug detected: "++)

labelAttr :: AttrName
labelAttr = fromString "code label"

-- | For addresses that we think are probably code pointers.
renderLabel :: Word16 -> Widget n
renderLabel = withAttr labelAttr . str . printf "%04x"

renderMemMapInfo :: UI -> Widget n
renderMemMapInfo ui = vBox
	[ renderMemMap "CPU" cpu
	, renderMemMap "PPU" ppu
	, renderMap "Maps" (renderMemMapName (selectedMemMapName ui) (visibleMemMapName ui)) (memMaps ui)
	, renderMap "Sources" renderChunk (memMapSources ui)
	] where
	renderMemMap memTyString memTyAccessor = renderMap
		memTyString
		(renderMemMapItem memTyString (memMapSources ui))
		(memTyAccessor (visibleMemMap ui))

emptyAttr :: AttrName
emptyAttr = fromString "empty"

title :: String -> Widget n
title s = hBox [hLimit 3 hBorder, str (" " ++ s ++ " "), hBorder]

-- | The first argument is a header/title.
renderMap :: String -> (k -> v -> Widget n) -> Map k v -> Widget n
renderMap s f m = vBox
	$ title s
	: if null m
	  then [withAttr emptyAttr $ str "    (no entries)"]
	  else map (uncurry f) (M.toAscList m)

renderChunk :: Digest SHA256 -> Chunk -> Widget n
renderChunk hash item =
	str (description item)
	<=>
	(str "    " <+> (renderSource (source item)
	                 <=>
	                 renderHash hash
	                )
	)

renderSource :: Source -> Widget n
renderSource = go id where
	go f (Zip fp s) = go (f . (fp++) . (':':)) s
	go f (File fp s) = str (f fp) <=> renderSlice s

renderMemMapItem :: String -> Chunks -> Word16 -> MemMapItem -> Widget n
renderMemMapItem memTy chunks start item = hBox
	[ renderSlice16 s { offset = fromIntegral start }
	, str " ▶ "
	, renderSlice16 s
	, str (" (" ++ source ++ ")")
	] where
	s = mmiSlice item
	describe chunk = ": " ++ description chunk
	source = case item of
		Mirror _ -> memTy
		Bytes _ hash _ -> take 8 (show hash) <> foldMap describe (M.lookup hash chunks)

renderMemMapName :: String -> String -> String -> MemMap -> Widget n
renderMemMapName selected visible current _ = str $ current ++ case (current == visible, current == selected) of
	(False, False) -> ""
	(False, True ) -> " (selected)"
	(True , False) -> " (visible)"
	(True , True ) -> " (selected, visible)"

renderSlice :: Slice -> Widget n
renderSlice s = str $ printf "0x%x-0x%x" (offset s) (toInteger (offset s) + toInteger (size s) - 1)

renderSlice16 :: Slice -> Widget n
renderSlice16 s = str $ printf "0x%04x-0x%04x" (offset s) (offset s+size s-1)

renderHash :: Digest SHA256 -> Widget n
renderHash = str . show
