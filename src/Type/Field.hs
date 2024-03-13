module Type.Field where

import Data.Aeson (Key, ToJSON)
import Data.Aeson.Key qualified as JK
import Data.Text qualified as Text
import GHC.Generics (Generic)

type Path = [Key]

data Field = Timestamp | Raw | Field Path
  deriving (Show, Eq, Ord, Generic, ToJSON)

drawLogsViewColumnHeaderTxt :: Field -> Text.Text
drawLogsViewColumnHeaderTxt = \case
  Timestamp -> "@timestamp"
  Raw -> "@raw"
  Field path -> drawPath path

drawPath :: [JK.Key] -> Text.Text
drawPath = Text.intercalate "." . map JK.toText
