module Column where

import Control.DeepSeq (NFData)
import Control.Lens ((%~))
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import GHC.Generics (Generic)
import Type.Field (Path)
import Type.MaxWidth (MaxWidth (..))

data Column = Column {path :: Path, width :: MaxWidth}
  deriving (Generic, Show, NFData)

valueColumns :: Value -> [Column]
valueColumns (Object o) = concatMap
  do \(Key.toText -> k, v) -> (#path %~ (k :)) <$> valueColumns v
  do KeyMap.toList o
valueColumns (String txt) =
  [Column [] . MaxWidth . Text.length $ txt]
valueColumns (Bool _) =
  [Column [] . MaxWidth $ 5]
valueColumns (Number n) =
  [Column [] . MaxWidth . F.length . show $ n]
valueColumns Null =
  [Column [] $ MaxWidth 4]
valueColumns (Array arr) =
  [ Column
      []
      . MaxWidth
      . fromIntegral
      . Text.Lazy.length
      . Text.Lazy.take 1000
      . Text.Lazy.decodeUtf8
      . encode
      $ arr
  ]
