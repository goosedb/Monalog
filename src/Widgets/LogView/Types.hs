module Widgets.LogView.Types where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Lens (Bifunctor (..))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withText)
import Data.Bool (bool)
import Data.JSONPath.Execute qualified as PE
import Data.JSONPath.Types (JSONPathElement)
import Data.JSONPath.Types qualified as PE
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Exts (IsList (..))
import GHC.Generics
import Graphics.Vty qualified as V
import Type.Name
import Type.WidgetSize (WidgetSize (Auto))
import Widgets.Editor (emptyEditor)
import Widgets.LogView.Tokenize (Token, prepare, tokenize, wrap, wrapText)

data TokenizedValue = TokenizedValue {value :: Value, tokens :: [Maybe Token]}

data LogViewWidget = LogViewWidget
  { selectedLog :: TokenizedValue
  , cache :: Maybe (Seq [Token])
  , offset :: Int
  , settings :: LogViewWidgetSettings
  }
  deriving (Generic)

data LogViewWidgetSettings = LogViewWidgetSettings
  { jsonPathEditor :: B.Editor Text Name
  , showJsonpath :: Bool
  , width :: WidgetSize
  , height :: WidgetSize
  , jsonpathFilter :: Either Text [JSONPathElement]
  , copyMethod :: CopyMethod
  , nativeCopyCmd :: Maybe String
  , textWrap :: Bool
  }
  deriving (Generic)

data CopyMethod = Native | Osc52

instance FromJSON CopyMethod where
  parseJSON = withText "CopyText" \case
    "osc52" -> pure Osc52
    "native" -> pure Native
    _ -> fail "Failed to parse copy method"

instance ToJSON CopyMethod where
  toJSON =
    String . \case
      Osc52 -> "osc52"
      Native -> "native"

newLogView :: Value -> LogViewWidgetSettings -> LogViewWidget
newLogView l settings =
  LogViewWidget
    { selectedLog = TokenizedValue l (tokenize l)
    , cache = Nothing
    , offset = 0
    , settings
    }

emptyLogWidgetSettings :: LogViewWidgetSettings
emptyLogWidgetSettings =
  LogViewWidgetSettings
    { jsonPathEditor = emptyEditor (mkName LogViewWidgetJsonpathEditor)
    , showJsonpath = False
    , jsonpathFilter = Right []
    , width = Auto
    , height = Auto
    , copyMethod = Osc52
    , nativeCopyCmd = Nothing
    , textWrap = True
    }

data LogViewWidgetEvent
  = LogSelected
  | Scroll Int
  | Click LogViewWidgetName B.Location B.Location
  | Move LogViewWidgetName B.Location B.Location
  | Key V.Key [V.Modifier]

data LogViewWidgetCallbacks s = LogViewWidgetCallbacks
  { copied :: B.EventM Name s ()
  , copyError :: Text -> B.EventM Name s ()
  }

mkName :: LogViewWidgetName -> Name
mkName = WidgetName . LogViewWidgetName

jsonpathPrefix :: Text
jsonpathPrefix = "$"

extractByJsonPath :: Value -> [PE.JSONPathElement] -> Value
extractByJsonPath value =
  (\case [x] -> x; a -> Array $ fromList a)
    . ($ value)
    . PE.executeJSONPath

tokenizeValue :: Value -> TokenizedValue
tokenizeValue v = TokenizedValue v (tokenize v)

generateCache :: Maybe Int -> TokenizedValue -> Either Text [JSONPathElement] -> Bool -> Either [Text] [[Token]]
generateCache w selectedLog jsonpathFilter showJsonpath = bimap
  do maybe pure wrapText w
  do tokenizeAndWrap w
  do bool (Right selectedLog) (tokenizeValue . extractByJsonPath selectedLog.value <$> jsonpathFilter) showJsonpath

tokenizeAndWrap :: Maybe Int -> TokenizedValue -> [[Token]]
tokenizeAndWrap w =
  prepare
    . maybe id wrap w
    . tokens
