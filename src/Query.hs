module Query where

import Data.Aeson (Key, Value (..))

data Query
  = And Query Query
  | Not Query
  | Or Query Query
  | Eq Query Query
  | Gt Query Query
  | Like Query Query
  | In Query Query
  | Path [Key]
  | Value Value
  deriving (Show)
