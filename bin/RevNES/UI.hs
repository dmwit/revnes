module RevNES.UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.List
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
	} deriving Show

selectedMemMap :: UI -> MemMap
selectedMemMap ui = M.findWithDefault def (selectedMemMapName ui) (memMaps ui)

visibleMemMap :: UI -> MemMap
visibleMemMap ui = M.findWithDefault def (visibleMemMapName ui) (memMaps ui)

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
		}

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
				continue (ui { commandLine = applyEdit clearZipper cl, messageLog = ml })
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
		, (severityAttr Warning, fg yellow)
		, (severityAttr Error  , fg red)
		, (emptyAttr, fg brightBlue)
		, (missingByteAttr, fg black <> bg red)
		, (openFoldAttr, fg yellow `withStyle` bold)
		]

renderTab :: UI -> Widget n
renderTab ui = case tab ui of
	CPU -> renderCPUMemory ui
	Maps -> renderMemMapInfo ui

renderCPUMemory :: UI -> Widget n
renderCPUMemory ui = vBox
	$ title "CPU memory"
	: renderedFrags
	where
	memMap = cpu (selectedMemMap ui)
	renderedFrags = concatMap (renderFrag ui) $ F.flatten memMap (Slice 0 (2^16))

renderFrag :: UI -> (Word16, FMF) -> [Widget n]
renderFrag ui ~(addr, frag) = concatMap renderFold summarized
	where
	foldItems = fmap (\(slice, foldID) ->
	            	( slice { offset = offset slice + fromIntegral addr }
	            	, foldContents ui IM.! foldID
	            	)
	            ) <$> F.view (folds ui) frag
	summarized = summarizeFolds (Slice (fromIntegral addr) (F.lengthFMF frag)) foldItems
	memMap = cpu $ selectedMemMap ui

	renderFold (slice, depth, Just fc) = return
		. renderRow depth (offset slice)
		$ withAttr closedFoldAttr (renderFoldContent (offset slice) fc)
	renderFold (Slice { size = 0 }, _, _) = []
	renderFold (slice, depth, Nothing) =
		let backing = MM.lookup (fromIntegral (offset slice)) memMap
		    slice' = memLoc backing
		    sz = min (size slice) (size slice')
		    nextSlice = Slice (offset slice + size slice') (size slice - sz)
		    remainingBytes = renderFold (nextSlice, depth, Nothing)
		    addrs = [offset slice .. offset slice + sz - 1]
		    rows = case backing of
		    	-- should be impossible, but...
		    	Empty slice' -> repeat unknownByte
		    	Backed { bytes = bs } -> withAttr openFoldAttr . str . printf "0x%02x" <$> BS.unpack bs
		in zipWith3 renderRow (repeat depth) addrs rows ++ remainingBytes

	unknownByte = withAttr missingByteAttr (str "???? (report a bug, please)")
	renderRow depth addr widget = str (printf "%s 0x%04x " (showDepth depth) addr) <+> widget

	renderFoldContent addr (User lbl) = str lbl
	-- TODO: not def
	renderFoldContent addr (Instruction i) = str (pp def (fromIntegral addr) i)

	showDepth n | n < 1 = " "
	            | n < 10 = show n
	            | otherwise = "+"

-- assumes each slice of the forest is completely contained in the first argument slice
summarizeFolds :: Slice -> Forest (Slice, FoldItem a) -> [(Slice, Int, Maybe a)]
summarizeFolds s [] = [(s, 0, Nothing) | size s > 0]
summarizeFolds s (Node (s', v) children:forest) = prefix ++ node ++ suffix where
	prefix = [(s { size = offset s' - offset s }, 0, Nothing) | offset s' > offset s]
	suffix = summarizeFolds (Slice (offset s' + size s') (offset s + size s - offset s' - size s')) forest
	node = if open v
	       then [(s, depth+1, ma) | (s, depth, ma) <- summarizeFolds s' children]
	       else [(s', 1, Just (item v))]

closedFoldAttr :: AttrName
closedFoldAttr = fromString "closed fold"

openFoldAttr :: AttrName
openFoldAttr = fromString "open fold"

missingByteAttr :: AttrName
missingByteAttr = fromString "missing byte"

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
