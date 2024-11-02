module Query.Parser where

import Control.Applicative (asum)
import Control.Monad.Combinators.Expr
import Data.Aeson (Value (..))
import Data.Either (isRight)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Query (Filter (..), Located (..), Pos (..), Query (..), Span (..))
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char as M
import Text.Megaparsec.Char.Lexer qualified as M
import Type.Field (Field (Field, Timestamp), specialFieldSymbols)
import Type.Sort

type Parser = Parsec Void Text

data Lexeme = LKey Text | LDot | LOther
  deriving (Show)

keyLexer :: Parser [Lexeme]
keyLexer = do
  a <- many (((LKey <$> keyParser) <|> (LDot <$ ".") <|> (LOther <$ stringP) <|> LOther <$ some (noneOf @[] "\n \t")) <* hspace)
  M.eof
  pure a

queryParser :: Parser Query
queryParser = do
  hspace
  f <- optional filterParser
  hspace
  s <- optional do
    "|" >> hspace >> sortParser
  hspace >> eof
  pure $ Query f (fromMaybe [] s)

sortParser :: Parser [(Sort, Located Field)]
sortParser = ((order <* hspace1) >>= \o -> (o,) <$> (fieldParserLocated <* hspace)) `sepBy` ("," >> hspace)
 where
  order = (Desc <$ "desc") <|> (Asc <$ "asc")

fieldParserLocated :: Parser (Located Field)
fieldParserLocated = do
  let special = "@" >> (Timestamp <$ "timestamp") <|> Field . pure . Text.pack . ('@' :) <$> some (oneOf specialFieldSymbols)
  located special <|> (fmap Field <$> pathParser)

fieldParser :: Parser Field
fieldParser = fmap (.value) fieldParserLocated

filterParser :: Parser Filter
filterParser = expr
 where
  expr = makeExprParser term table <* hspace
  braced = between ("(" <* hspace) (")" <* hspace)

  term =
    braced expr
      <|> (Value <$> valueParser <* hspace)
      <|> (Path <$> pathParser <* hspace)
      <|> (Query.Array <$> array <* hspace)

  array = between
    do "[" >> hspace
    do "]" >> hspace
    do (expr <* hspace) `sepBy` ("," >> hspace)

  table :: [[Operator Parser Filter]]
  table =
    [ [prefix "not" Not]
    ,
      [ binary "=" Eq
      , binary "!=" Neq
      , binary ">=" Gte
      , binary "<=" Lte
      , binary ">" Gt
      , binary "<" Lt
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
      , Number <$> M.signed (pure ()) M.scientific
      ]

located :: Parser a -> Parser (Located a)
located p = do
  let toPos M.SourcePos{..} = Pos{column = M.unPos sourceColumn, row = M.unPos sourceLine}
  begin <- toPos <$> M.getSourcePos
  v <- p
  end <- toPos <$> M.getSourcePos
  pure (Located v (Span begin end))

pathParser :: Parser (Located [Text])
pathParser = located do keyParser `sepBy1` "."

stringP :: Parser Text
stringP = p '\'' <|> p '\"'
 where
  p :: Char -> Parser Text
  p delim =
    between
      (fromString [delim])
      (fromString [delim])
      (Text.concat <$> many (M.string ("\\\\" <> Text.pack [delim]) $> ("\\" <> Text.pack [delim]) <|> Text.singleton <$> M.noneOf [delim]))

keyParser :: Parser Text
keyParser = simpleKey <|> "$" *> stringP

simpleKey :: Parser Text
simpleKey = fmap Text.pack $ (:) <$> oneOf simpleKeyCharsFirst <*> many (oneOf simpleKeyChars)
 where
  simpleKeyChars :: Set Char
  simpleKeyChars = simpleKeyCharsFirst <> Set.fromList ['0' .. '9']

  simpleKeyCharsFirst :: Set Char
  simpleKeyCharsFirst = Set.fromList $ '_' : '-' : ['a' .. 'z'] <> ['A' .. 'Z']

isSimpleKey :: Text -> Bool
isSimpleKey = isRight . runParser simpleKey ""
