module Type.Field where

import Data.Aeson (ToJSON)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

type Path = [Text]

data Field = Timestamp | Field Path
  deriving (Show, Eq, Ord, Generic, ToJSON)

drawLogsViewColumnHeaderTxt :: Field -> Text.Text
drawLogsViewColumnHeaderTxt = \case
  Timestamp -> timestampName
  Field path -> textPath path

timestampName :: Text
timestampName = "@timestamp"

specialFieldSymbols :: Set Char
specialFieldSymbols = Set.fromList ['a' .. 'z']

textPath :: Path -> Text.Text
textPath = Text.intercalate "."
