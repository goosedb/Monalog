module Query.Parser where

import Control.Applicative (asum)
import Control.Monad.Combinators.Expr
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Exts (IsList (..))
import Query (Query (..))
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char qualified as M
import Text.Megaparsec.Char.Lexer qualified as M

type Parser = Parsec Void Text

queryParser :: Parser Query
queryParser = hspace *> expr <* eof
 where
  expr = makeExprParser term table <* hspace
  pathParser = sepBy1 @Parser keyParser "."

  braced = between ("(" <* hspace) (")" <* hspace)

  term =
    braced expr
      <|> (Value <$> valueParser <* hspace)
      <|> (Path <$> pathParser <* hspace)

  table :: [[Operator Parser Query]]
  table =
    [ [prefix "not" Not]
    ,
      [ binary "=" Eq
      , binary "!=" (\a b -> Not (Eq a b))
      , binary ">=" (\a b -> Gt a b `Or` Eq a b)
      , binary "<=" (\a b -> Gt b a `Or` Eq a b)
      , binary ">" Gt
      , binary "<" (flip Gt)
      , binary "like" Like
      , binary "in" In
      ]
    , [binary "&&" And]
    , [binary "||" Or]
    ]
  binary name f = InfixL (f <$ (string name <* hspace))
  prefix name f = Prefix (f <$ (string name <* hspace))

  valueParser :: Parser Value
  valueParser =
    asum
      [ Bool True <$ "true"
      , Bool False <$ "false"
      , Null <$ "null"
      , String <$> stringP
      , Number . fromRational . toRational @Double <$> M.signed (pure ()) (try M.float <|> M.decimal)
      , Array . fromList <$> between
          do "[" >> hspace
          do "]" >> hspace
          do (valueParser <* hspace) `sepBy` ("," >> hspace)
      ]

  stringP = between "\"" "\"" (Text.concat <$> many ((M.string "\\\"" $> "\"") <|> (Text.singleton <$> M.noneOf ['\"'])))

  keyParser = Key.fromText <$> (simpleKey <|> ("$" *> stringP))
   where
    simpleKey = fmap Text.pack $ (:) <$> oneOf a1 <*> many (oneOf a2)
    a1 = '_' : ['a' .. 'z'] <> ['A' .. 'Z']
    a2 = a1 <> ['0' .. '9']
