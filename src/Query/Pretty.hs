module Query.Pretty where

import Data.Aeson qualified as J
import Data.Foldable (Foldable (foldl'))
import Data.Scientific qualified as S
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Query
import Query.Parser (isSimpleKey)
import Type.Field (Field (..), specialFieldSymbols)
import Type.Sort (Sort (..))

queryPretty :: Query -> Text
queryPretty q =
  maybe "" (flip Text.append " " . filterPretty) q.filter <> case q.sort of
    [] -> ""
    s -> "| " <> sortPretty s

sortPretty :: SortList -> Text
sortPretty = Text.intercalate ", " . map \(s, Located{value}) -> orderPretty s <> " " <> fieldPretty value
 where
  orderPretty = \case
    Desc -> "desc"
    Asc -> "asc"

fieldPretty :: Field -> Text
fieldPretty = \case
  Timestamp -> "@timestamp"
  Field p -> pathPretty p

filterPretty :: Filter -> Text
filterPretty = go
 where
  go e = case e of
    And l r -> prettyBin "&&" e l r
    Not v -> "not " <> (if isAtom v then id else braced) (go v)
    Or l r -> prettyBin "||" e l r
    Eq l r -> prettyBin "=" e l r
    Gt l r -> prettyBin ">" e l r
    Neq l r -> prettyBin "!=" e l r
    Lt l r -> prettyBin "<" e l r
    Lte l r -> prettyBin "<=" e l r
    Gte l r -> prettyBin ">=" e l r
    Like l r -> prettyBin "like" e l r
    Ilike l r -> prettyBin "ilike" e l r
    In l r -> prettyBin "in" e l r
    Path Located{value} -> pathPretty value
    Array vs -> "[" <> Text.intercalate ", " (map go vs) <> "]"
    JsonValue v -> prettyValue v
    StringValue txt _ -> prettyString txt

  lessPriority r e = priority r > priority e
  braceOfLessPriority p e = (if lessPriority p e then braced else id) (go e)
  prettyBin sym e l r = braceOfLessPriority e l <> " " <> sym <> " " <> braceOfLessPriority e r

  priority = \case
    And _ _ -> (1 :: Int)
    Not _ -> maxBound
    Or _ _ -> 0
    Eq _ _ -> 2
    Gt _ _ -> 2
    Like _ _ -> 2
    Ilike _ _ -> 2
    In _ _ -> 2
    Path _ -> maxBound
    Array _ -> maxBound
    JsonValue _ -> maxBound
    StringValue _ _ -> maxBound
    Neq _ _ -> maxBound
    Lt _ _ -> 2
    Lte _ _ -> 2
    Gte _ _ -> 2

  braced a = "(" <> a <> ")"
  prettyValue = \case
    J.Bool True -> "true"
    J.Bool False -> "false"
    J.Null -> "null"
    J.String t -> "\"" <> escape t <> "\""
    J.Number n -> Text.pack . either @Double @_ @Int show show . S.floatingOrInteger $ n
    _ -> ""
  prettyString txt = "\"" <> escape txt <> "\""

isAtom :: Filter -> Bool
isAtom = \case
  And _ _ -> False
  Not _ -> False
  Or _ _ -> False
  Eq _ _ -> False
  Gt _ _ -> False
  Like _ _ -> False
  Ilike _ _ -> False
  In _ _ -> False
  Neq _ _ -> False
  Lt _ _ -> False
  Lte _ _ -> False
  Gte _ _ -> False
  Path _ -> True
  Array _ -> True
  JsonValue _ -> True
  StringValue _ _ -> True

pathPretty :: [Text] -> Text
pathPretty [p] | Just ('@', xs) <- Text.uncons p, Text.all (`Set.member` specialFieldSymbols) xs = p
pathPretty path = Text.intercalate "." (map pathPiecePretty path)

pathPiecePretty :: Text -> Text
pathPiecePretty p = if isSimpleKey p then p else "$\"" <> escape p <> "\""

escape :: Text -> Text
escape = Text.pack . reverse . foldl' (\acc c -> if c == '"' then '"' : '\\' : acc else c : acc) [] . Text.unpack
