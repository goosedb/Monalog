{-# LANGUAGE DerivingStrategies #-}
module Type.Event where

import Data.Text (Text)
import Type.Log (Log)
import Control.Exception (Exception)

data Event = NewLog Log | FilteredLog Log | SearchEnded | Tick | FatalError FatalError

newtype FatalError = MkFatalError Text
  deriving Show
  deriving anyclass Exception