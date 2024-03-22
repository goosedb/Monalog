{-# LANGUAGE DerivingStrategies #-}

module Type.MaxWidth where

import Data.Aeson
import GHC.Generics

newtype MaxWidth = MaxWidth {rawMaxWidth :: Int}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)
