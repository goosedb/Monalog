{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import App
import Data.Maybe (fromMaybe)
import Options.Applicative
import Options.Applicative.Common (runParser)

parseArgs :: Parser AppArguments
parseArgs = do
  input <-
    fmap (fromMaybe Stdin)
      . optional
      . fmap File
      $ argument
        str
        ( metavar "FILE"
            <> help
              "The format can be derived from the file format. \
              \If the file format is .csv, the output will be csv, otherwise it will be json."
        )

  format <-
    optional
      . option
        ( maybeReader \case
            "json" -> Just Json
            "csv" -> Just Csv
            _ -> Nothing
        )
      $ ( long "format"
            <> short 'f'
            <> metavar "FORMAT"
            <> help "Supported formats: json, csv. In the case of csv, the first line is considered the header."
        )

  mbDefaultField <- optional (strOption (long "default-field" <> short 'd' <> metavar "FIELD"))
  configPath <- optional (strOption (long "config" <> short 'c' <> metavar "CONFIG"))
  ignoreConfig <- switch (long "ignore-config" <> short 'i' <> help "Makes monalog ignore config")
  pure $ AppArguments{..}

main :: IO ()
main = execParser (info (parseArgs <**> helper) fullDesc) >>= app
