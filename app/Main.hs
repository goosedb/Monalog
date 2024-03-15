{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import App
import Data.Maybe (fromMaybe)
import Options.Applicative
import Options.Applicative.Common (runParser)

parseArgs :: Parser AppArguments
parseArgs =
  AppArguments
    <$> ( fromMaybe Stdin
            <$> optional (File <$> argument str (metavar "FILE"))
        )
    <*> optional
      ( option
          ( maybeReader \case
              "json" -> Just Json
              "csv" -> Just Csv
              _ -> Nothing
          )
          (long "format" <> short 'f')
      )

main :: IO ()
main = execParser (info (parseArgs <**> helper) fullDesc) >>= app
