module Path where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Foldable qualified as F
import Data.Maybe (fromMaybe)

getByPath :: (Foldable t) => t Key -> Value -> Value
getByPath path val = fromMaybe Null $ F.foldl' (\v k -> v >>= (^? key k)) (Just val) path
