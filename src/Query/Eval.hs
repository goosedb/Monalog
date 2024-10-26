module Query.Eval where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import Data.Foldable qualified as F
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Query (Query (..))

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
    Path path -> ValueResult (getByPath path v)
    Value val -> ValueResult val
    Like l r -> BoolResult case (go l, go r) of
      (ValueResult (String ls), ValueResult (String lr)) -> lr `Text.isInfixOf` ls
      _ -> False
    In a as -> case (toValue (go a), toArray (go as)) of
      (a', as') -> BoolResult $ elem a' as'
    Gt ql qr -> BoolResult case (toValue $ go ql, toValue $ go qr) of
      (Bool l, Bool r) -> l > r
      (Number l, Number r) -> l > r
      (String l, String r) -> l > r
      _ -> False

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

getByPath :: (Foldable t) => t Key -> Value -> Value
getByPath path val = fromMaybe Null $ F.foldl' (\v k -> v >>= (^? key k)) (Just val) path
