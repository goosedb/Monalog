module Type.LogViewPosition where

import Data.Aeson

data LogViewPosition
  = LogViewPositionBottom
  | LogViewPositionRight

instance FromJSON LogViewPosition where
  parseJSON = withText "LogViewPosition" \case
    "right" -> pure LogViewPositionRight
    "bottom" -> pure LogViewPositionBottom
    _ -> fail "Failed to parse log view side"

instance ToJSON LogViewPosition where
  toJSON = \case
    LogViewPositionRight -> "right"
    LogViewPositionBottom -> "bottom"
