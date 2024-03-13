module Type.Config where

import Data.Aeson (Key)
import Data.List.NonEmpty qualified as Nel

data Separator = Newline

data Config = Config
  { separator :: Separator
  , defaultField :: Nel.NonEmpty Key
  }
