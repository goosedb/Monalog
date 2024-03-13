module Type.With where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data With a b = With {essence :: a, extra :: b}
  deriving (Generic, Show, ToJSON, Eq, Ord)
