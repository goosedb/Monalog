module Query where

import Data.Aeson (Value (..))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Type.Field (Field, Path)
import Type.Sort

data Pos = Pos {row :: Int, column :: Int}
  deriving (Show, Eq, Ord)

isIn :: Pos -> Span -> Bool
isIn pos Span{..} = pos >= from && pos <= to

data Span = Span {from :: Pos, to :: Pos}
  deriving (Show)

dummySpan :: Span
dummySpan = Span dummyPos dummyPos where dummyPos = Pos 0 0

data Located a = Located {value :: a, span :: Span}
  deriving (Show, Functor)

dummyLocated :: a -> Located a
dummyLocated = flip Located dummySpan

mkStringValue :: Text -> Filter
mkStringValue txt = StringValue txt (Text.toLower txt)

data Filter
  = And Filter Filter
  | Not Filter
  | Or Filter Filter
  | Eq Filter Filter
  | Neq Filter Filter
  | Gt Filter Filter
  | Lt Filter Filter
  | Lte Filter Filter
  | Gte Filter Filter
  | Like Filter Filter
  | Ilike Filter Filter
  | In Filter Filter
  | Path (Located Path)
  | Array [Filter]
  | JsonValue Value
  | StringValue Text Text
  deriving (Show)

data Query = Query
  { filter :: Maybe Filter
  , sort :: SortList
  }
  deriving (Show, Generic)

type SortList = [(Sort, Located Field)]
