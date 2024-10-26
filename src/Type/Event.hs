{-# LANGUAGE DerivingStrategies #-}

module Type.Event where

import Control.Concurrent (ThreadId)
import Control.Exception (Exception)
import Data.Text (Text)
import Type.Log (Log)

data Event = NewLogs [Log] | FilteredLogs [Log] | ResetStatus ThreadId | FatalError FatalError

newtype FatalError = MkFatalError Text
  deriving (Show)
  deriving anyclass (Exception)
