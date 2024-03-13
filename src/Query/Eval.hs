module Query.Eval where

import Data.Aeson (Value (..))
import Data.Foldable qualified as F
import Path (getByPath)
import Query (Query (..))
import Text.Fuzzy qualified as Fuzzy

data QueryResult = BoolResult Bool | ValueResult Value
  deriving (Show)

evalQuery :: Value -> Query -> QueryResult
evalQuery v = go
 where
  go = \case
    And ql qr -> BoolResult $ toBool (go ql) && toBool (go qr)
    Or ql qr -> BoolResult $ toBool (go ql) || toBool (go qr)
    Not q' -> BoolResult $ not $ toBool $ go q'
    Eq ql qr -> BoolResult case (go ql, go qr) of
      (BoolResult l, BoolResult r) -> l && r
      (BoolResult l, ValueResult r) -> Bool l == r
      (ValueResult lval, ValueResult rval) -> lval == rval
      (ValueResult l, BoolResult r) -> l == Bool r
    Zoom path q' -> evalQuery (getByPath path v) q'
    Path path -> ValueResult (getByPath path v)
    Value val -> ValueResult val
    Like l r -> BoolResult case (go l, go r) of
      (ValueResult (String ls), ValueResult (String lr)) -> Fuzzy.test ls lr
      _ -> False
    In a as -> case (toValue (go a), toArray (go as)) of
      (a', as') -> BoolResult $ elem a' as'

  toValue = \case
    BoolResult a -> Bool a
    ValueResult a -> a

  toArray = \case
    ValueResult (Array a) -> F.toList a
    _ -> []

toBool :: QueryResult -> Bool
toBool = \case
  BoolResult b -> b
  ValueResult (Bool b) -> b
  ValueResult _ -> False
