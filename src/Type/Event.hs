{-# LANGUAGE DerivingStrategies #-}

module Type.Event where

import Column
import Control.Concurrent (ThreadId)
import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Data.Text (Text)
import GHC.Generics (Generic)
import Type.Field (Field)
import Type.Log (Log, Logs, Mutable)

data NewLog = NewLog {log :: Log, columns :: [Column]}
  deriving (Show, Generic, NFData)

data Event
  = NewLogs [NewLog]
  | SelectFields [Field]
  | FilteredLogs (Either (Logs Mutable) [Log])
  | ResetStatus ThreadId
  | FatalError FatalError

newtype FatalError = MkFatalError Text
  deriving (Show)
  deriving anyclass (Exception)
