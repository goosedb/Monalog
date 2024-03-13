module Type.AvailableSpace where

import GHC.Generics (Generic)

data AvailableSpace = AvailableSpace
  { width :: Int
  , height :: Int
  }
  deriving (Generic)
