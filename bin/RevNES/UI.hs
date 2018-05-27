module RevNES.UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.List
import Data.Char
import Data.Default
import Data.Maybe
import Data.Map (Map)
import Data.Monoid
import Data.String
import Data.Text.Zipper
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Text.Printf
import qualified Data.Vector as V
import qualified Data.Map as M

import RevNES.MemMap

data Focus = CommandLine | MessageLog
	deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

data UI = UI
	{ focus :: Focus
	, commandLine :: Editor String Focus
	, messageLog :: List Focus Message
	, memMapSources :: Chunks
	, memMaps :: MemMaps
	, selectedMemMap, visibleMemMap :: String
	} deriving Show

instance Default UI where
	def = UI
		{ focus = CommandLine
		, commandLine = editor CommandLine (Just 1) ""
		, messageLog = list MessageLog V.empty 1
		, memMapSources = M.empty
		, memMaps = M.empty
		, selectedMemMap = "default"
		, visibleMemMap = "default"
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
		<=> renderMemMapInfo ui

	handleEvent ui e = case e of
		VtyEvent (EvKey (KChar '1') [MMeta]) -> continue (ui { focus = CommandLine })
		VtyEvent (EvKey (KChar '2') [MMeta]) -> continue (ui { focus = MessageLog  })
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
		]

renderMemMapInfo :: UI -> Widget n
renderMemMapInfo ui = vBox
	[ renderMemMap "CPU" cpu
	, renderMemMap "PPU" ppu
	, renderMap "Maps" (renderMemMapName (selectedMemMap ui) (visibleMemMap ui)) (memMaps ui)
	, renderMap "Sources" renderChunk (memMapSources ui)
	] where
	visibleMap = M.findWithDefault def (visibleMemMap ui) (memMaps ui)
	renderMemMap memTyString memTyAccessor = renderMap
		memTyString
		(renderMemMapItem memTyString (memMapSources ui))
		(memTyAccessor visibleMap)

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
