module Widgets.LogView.Types where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Aeson (Value (..))
import Data.Text (Text)
import GHC.Exts (IsList (..))
import GHC.Generics
import Graphics.Vty qualified as V
import Type.Log (Log (..))
import Type.Name
import Type.WidgetSize (WidgetSize (Auto))
import Widgets.Editor (emptyEditor)

data LogViewWidget = LogViewWidget
  { selectedLog :: Maybe Log
  , jsonPathEditor :: B.Editor Text Name
  , showJsonpath :: Bool
  , width :: WidgetSize
  , height :: WidgetSize
  , jsonpathFilteredValue :: Either Text Value
  }
  deriving (Generic)

emptyLogWidget :: LogViewWidget
emptyLogWidget =
  LogViewWidget
    { selectedLog = Nothing
    , jsonPathEditor = emptyEditor (mkName LogViewWidgetJsonpathEditor)
    , showJsonpath = False
    , jsonpathFilteredValue = Right (Array $ fromList [])
    , width = Auto
    , height = Auto
    }

data LogViewWidgetEvent
  = LogSelected Log
  | Scroll Int
  | Click LogViewWidgetName B.Location B.Location
  | Move LogViewWidgetName B.Location B.Location
  | Key V.Key [V.Modifier]

newtype LogViewWidgetCallbacks s = LogViewWidgetCallbacks
  {copied :: B.EventM Name s ()}

mkName :: LogViewWidgetName -> Name
mkName = WidgetName . LogViewWidgetName

jsonpathPrefix :: Text
jsonpathPrefix = "$"
