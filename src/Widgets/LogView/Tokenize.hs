module Widgets.LogView.Tokenize where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as Text
import GHC.IsList (IsList (..))

data Token = Token
  { jsonpath :: [Text.Text]
  , text :: Text.Text
  , kind :: TokenKind
  }
  deriving (Show)

data TokenKind = Key | Number | String | Keyword | Bullet | Normal | Plus
  deriving (Show)

data Inside = InsideObject | InsideArray
  deriving (Eq)

prepare :: [Maybe Token] -> [[Token]]
prepare = fromList . go []
 where
  go acc = \case
    Just a : as -> go (a : acc) as
    Nothing : as -> reverse acc : go [] as
    [] -> []

wrapText :: Int -> Text.Text -> [Text.Text]
wrapText maxWidth = go maxWidth . Text.lines
 where
  go _ [] = []
  go _ ("" : ts) = "" : go maxWidth ts
  go rest (text : ts) =
    let txtLen = Text.length text
        (a, b) = Text.splitAt rest text
     in if txtLen > rest
          then a : go maxWidth (b : ts)
          else text : go (rest - txtLen) ts

wrap :: Int -> [Maybe Token] -> [Maybe Token]
wrap maxWidth = go maxWidth
 where
  go _ [] = []
  go _ (Nothing : ts) = Nothing : go maxWidth ts
  go rest (Just t@Token{..} : ts) =
    let txtLen = Text.length text
        (a, b) = Text.splitAt rest text
     in if txtLen > rest
          then Just Token{text = a, ..} : Nothing : go maxWidth (Just Token{text = b, ..} : ts)
          else Just t : go (rest - txtLen) ts

tokenize :: J.Value -> [Maybe Token]
tokenize = go ["$"] 0 []
 where
  isAtom = \case
    J.Object _ -> False
    J.Array _ -> False
    _ -> True

  go jpath indent inside = \case
    J.Object obj -> case KeyMap.toList obj of
      kv : kvs ->
        let
          drawKV (Key.toText -> k, v) =
            let jpath' = jpath <> ["." <> k]
                tok' t kind = Just $ Token jpath' t kind
                plus = if isAtom v && InsideArray `notElem` inside then (tok' "+ " Plus :) else id
             in plus $ tok' k Key : tok' ": " Normal : go jpath' (indent + 2) (InsideObject : inside) v
          tabluated = (tok (Text.pack $ replicate indent ' ') Normal :)
          drawnFirst = case inside of
            InsideObject : _ -> newline : tabluated (drawKV kv)
            _ -> drawKV kv
         in
          drawnFirst <> foldMap (tabluated . drawKV) kvs
      _ -> atom "{}" Normal
    J.Array arr -> case zip [0 :: Int ..] (toList arr) of
      iv : ivs ->
        let
          drawIV (Text.pack . show -> i, v) =
            let jpath' = jpath <> ["[" <> i <> "]"]
                tok' t kind = Just $ Token jpath' t kind
             in tok' "-" Bullet : tok' " " Normal : go jpath' (indent + 2) (InsideArray : inside) v
          tabluated = (tok (Text.pack $ replicate indent ' ') Normal :)
          drawnFirst = case inside of
            InsideArray : _ -> drawIV iv
            _ -> newline : tabluated (drawIV iv)
         in
          drawnFirst <> foldMap (tabluated . drawIV) ivs
      _ -> atom "{}" Normal
    J.String s -> case Text.lines s of
      ls@(_ : _ : _) ->
        tok ">" Normal
          : newline
          : foldMap (\l -> atom (Text.pack (replicate indent ' ') <> l) String) ls
      _ -> atom s String
    J.Number n -> atom (scientific n) Number
    J.Bool b -> atom (bool "false" "true" b) Keyword
    J.Null -> atom "null" Keyword
   where
    tok t k = Just $ Token jpath t k
    newline = Nothing
    atom t k = [tok t k, newline]
    scientific = Text.pack . either show show . floatingOrInteger @Double @Integer
