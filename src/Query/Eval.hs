module Query.Eval where

import Control.Lens
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Lens (key)
import Data.Foldable qualified as F
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IsList (IsList (fromList))
import Query (Filter (..), Located (..), SortList)
import Type.Field (Field (..))
import Type.Log
import Type.Sort (Sort (..))

data FilterResult = BoolResult Bool | ValueResult Value
  deriving (Show)

evalFilter :: Value -> Filter -> FilterResult
evalFilter v = go
 where
  go = \case
    And ql qr -> BoolResult $ toBool (go ql) && toBool (go qr)
    Or ql qr -> BoolResult $ toBool (go ql) || toBool (go qr)
    Not q' -> BoolResult $ not . toBool . go $ q'
    Eq ql qr -> BoolResult (eq ql qr)
    Path (Located{value = path}) -> ValueResult (getByPath path v)
    Value val -> ValueResult val
    Query.Array arr -> ValueResult $ Data.Aeson.Array $ fromList $ map (toValue . go) arr
    Like l r -> BoolResult case (go l, go r) of
      (ValueResult (String ls), ValueResult (String lr)) -> lr `Text.isInfixOf` ls
      _ -> False
    In a as -> case (toValue (go a), toArray (go as)) of
      (a', as') -> BoolResult $ elem a' as'
    Gt ql qr -> BoolResult (gt ql qr)
    Lt ql qr -> BoolResult (gt qr ql)
    Neq ql qr -> BoolResult $ not (eq ql qr)
    Lte ql qr -> BoolResult $ gt qr ql || eq qr qr
    Gte ql qr -> BoolResult $ gt ql qr || eq qr qr

  gt a b = case (toValue $ go a, toValue $ go b) of
    (Bool l, Bool r) -> l > r
    (Number l, Number r) -> l > r
    (String l, String r) -> l > r
    _ -> False

  eq a b = case (go a, go b) of
    (BoolResult l, BoolResult r) -> l && r
    (BoolResult l, ValueResult r) -> Bool l == r
    (ValueResult lval, ValueResult rval) -> lval == rval
    (ValueResult l, BoolResult r) -> l == Bool r

  toValue = \case
    BoolResult a -> Bool a
    ValueResult a -> a

  toArray = \case
    ValueResult (Data.Aeson.Array a) -> F.toList a
    _ -> []

toBool :: FilterResult -> Bool
toBool = \case
  BoolResult b -> b
  ValueResult (Bool b) -> b
  _ -> False

getByPath :: (Foldable t, Functor t) => t Text -> Value -> Value
getByPath path val = fromMaybe Null $ F.foldl' (\v k -> v >>= (^? key k)) (Just val) $ fmap Key.fromText path

compareLogs :: SortList -> Log -> Log -> Ordering
compareLogs [] _ _ = EQ
compareLogs ((order, Located{value = field}) : rest) a b =
  let
    compareFun :: (Ord a) => a -> a -> Ordering
    compareFun = case order of Desc -> flip compare; Asc -> compare
    compareResult = case field of
      Timestamp -> (compareFun `on` (.timestamp)) a b
      Field path -> (compareFun `on` (getByPath path . (.value))) a b
   in
    case compareResult of
      EQ -> compareLogs rest a b
      _ -> compareResult
