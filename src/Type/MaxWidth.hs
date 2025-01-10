{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Type.MaxWidth where

import Control.DeepSeq (NFData)
import Data.Aeson
import GHC.Generics

newtype MaxWidth = MaxWidth {rawMaxWidth :: Int}
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, NFData)
