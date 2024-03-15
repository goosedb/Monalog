{-# LANGUAGE DerivingStrategies #-}
module Type.Log where

import Data.Aeson (Value)
import Data.Time (UTCTime)
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MVec
import GHC.Generics (Generic)

newtype Idx = Idx { rawIdx :: Int }
  deriving newtype (Show, Eq, Ord)
data Log = Log {idx :: Idx, timestamp :: UTCTime, value :: Value}
  deriving (Show)

data Mutable

data Immutable

data family Logs m

data instance Logs Mutable = MutableLogs {logs :: MVec.IOVector Log, len :: Int}
  deriving (Generic)

newtype instance Logs Immutable = ImmutableLogs {logs :: Vec.Vector Log}
  deriving (Generic)
