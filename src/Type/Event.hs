{-# LANGUAGE DerivingStrategies #-}

module Type.Event where

import Control.Exception (Exception)
import Data.Text (Text)
import Type.Log (Log)

data Event = NewLogs [Log] | FilteredLogs [Log] | ResetCopied | FatalError FatalError

newtype FatalError = MkFatalError Text
  deriving (Show)
  deriving anyclass (Exception)
