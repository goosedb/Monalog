{-# LANGUAGE DerivingStrategies #-}

module Type.Event where

import Control.Exception (Exception)
import Data.Text (Text)
import Type.Log (Log)
import Control.Concurrent (ThreadId)

data Event = NewLogs [Log] | FilteredLogs [Log] | ResetStatus ThreadId | FatalError FatalError

newtype FatalError = MkFatalError Text
  deriving (Show)
  deriving anyclass (Exception)
