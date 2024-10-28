module Query where

import Data.Aeson (Key, Value (..))

data Pos = Pos {row :: Int, column :: Int}
  deriving (Show, Eq, Ord)

isIn :: Pos -> Span -> Bool
isIn pos Span{..} = pos >= from && pos <= to

data Span = Span {from :: Pos, to :: Pos}
  deriving (Show)

data Located a = Located {value :: a, span :: Span}
  deriving (Show)

data Query
  = And Query Query
  | Not Query
  | Or Query Query
  | Eq Query Query
  | Gt Query Query
  | Like Query Query
  | In Query Query
  | Path (Located [Key])
  | Value Value
  deriving (Show)
