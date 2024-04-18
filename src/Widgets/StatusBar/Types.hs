module Widgets.StatusBar.Types where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Type.LogViewPosition (LogViewPosition)
import Type.Name
import Control.Concurrent (ThreadId)

data StatusBarWidget = StatusBarWidget
  { totalLines :: Int
  , topLine :: Int
  , topLineEditor :: B.Editor Text Name
  , isEditorActive :: Bool
  , logViewPosition :: LogViewPosition
  , followLogs :: Bool
  , status :: StatusBarStatus
  }
  deriving (Generic)

data StatusBarStatus
  = Idle
  | JustCopied ThreadId
  | JustSavedConfig ThreadId

data StatusBarWidgetEvent
  = NewLog
  | ChangeTotalLines Int
  | ChangeTopLine Int
  | Click StatusBarWidgetName
  | Key V.Key [V.Modifier]
  | ResetFollow
  | SetCopied
  | ResetStatus ThreadId
  | ActivateEditor
  | ConfigSaved

data StatusBarWidgetCallbacks s = StatusBarWidgetCallbacks
  { goToTop :: B.EventM Name s ()
  , goToBottom :: B.EventM Name s ()
  , goTo :: Int -> B.EventM Name s ()
  , changeFollowLogs :: Bool -> B.EventM Name s ()
  }

mkName :: StatusBarWidgetName -> Name
mkName = WidgetName . StatusBarWidgetName
