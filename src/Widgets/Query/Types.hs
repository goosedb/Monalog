module Widgets.Query.Types where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Query
import Type.Name

data QueryWidget = QueryWidget
  { input :: B.Editor Text Name
  , parseError :: Maybe Text
  }
  deriving (Generic)

data QueryWidgetCallbacks s = QueryWidgetCallbacks
  { execFilter :: Query -> B.EventM Name s ()
  , clearFilter :: B.EventM Name s ()
  , showError :: Text -> B.EventM Name s ()
  }

data QueryWidgetEvent
  = Key V.Key [V.Modifier]
  | Click QueryWidgetName B.Location

mkName :: QueryWidgetName -> Name
mkName = WidgetName . QueryWidgetName

queryWidgetName :: Name
queryWidgetName = mkName QueryWidgetItself
