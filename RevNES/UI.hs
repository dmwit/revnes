module RevNES.UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.List
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Text.Zipper
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import qualified Data.Vector as V

data Focus = CommandLine | MessageLog | MemMapSources | MemMaps | MemMap
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
	-- , memMapSources :: UIMap () (Digest SHA256) ContextItem
	-- , memMaps :: UIMap () String MemMap
	-- , memMapView :: List () (Bool, Word16, MemMapItem)
	-- , memMapModel :: MemMap
	} deriving Show

defaultUI :: UI
defaultUI = UI
	{ focus = CommandLine
	, messageLog = list MessageLog V.empty 1
	, commandLine = editor CommandLine (Just 1) ""
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

	draw ui = joinBorders . border
		 $            renderEditor (vBox . map str)    (focus ui == CommandLine) (commandLine ui)
		<=> hBorder
		<=> vLimit 3 (renderList (const renderMessage) (focus ui == MessageLog ) (messageLog  ui))

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
		_ -> continue ui

	style = attrMap defAttr
		[ (listSelectedFocusedAttr, currentAttr `withStyle` bold)
		, (severityAttr Success, fg green)
		, (severityAttr Warning, fg yellow)
		, (severityAttr Error  , fg red)
		]

-- data UIMap n k v = UIMap
-- 	{ view :: List n (k, v)
-- 	, model :: Map k v
-- 	} deriving Show
