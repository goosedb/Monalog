module Query.Parser where

import Control.Applicative (asum)
import Control.Monad.Combinators.Expr
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.IsList (IsList (..))
import Query (Query (..))
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

queryParser :: Parser Query
queryParser = hspace *> expr <* eof
 where
  expr = makeExprParser term table <* hspace
  zoomParser = do
    string @_ @_ @Parser "zoom" >> hspace1
    path <- pathParser
    hspace
    query <- braced expr
    pure $ Zoom path query
  pathParser = sepBy1 @Parser keyParser "."

  braced = between ("(" <* hspace) (")" <* hspace)

  term =
    braced expr
      <|> (Value <$> valueParser <* hspace)
      <|> zoomParser
      <|> (Path <$> pathParser <* hspace)

  table :: [[Operator Parser Query]]
  table =
    [ [prefix "not" Not]
    ,
      [ binary "=" Eq
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
      , String . Text.pack <$> between "\"" "\"" (many (noneOf ['\"']))
      , Array . fromList <$> between
          do "[" >> hspace
          do "]" >> hspace
          do (valueParser <* hspace) `sepBy` ("," >> hspace)
      ]

  keyParser = fmap (Key.fromText . Text.pack) $ (:) <$> oneOf a1 <*> many (oneOf a2)
   where
    a1 = '_' : ['a' .. 'z'] <> ['A' .. 'Z']
    a2 = a1 <> ['0' .. '9']
