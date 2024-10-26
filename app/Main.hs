{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import App
import Config
import Control.Exception (catch)
import Control.Lens (set)
import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.IO.IOMode (IOMode (ReadMode))
import Options.Applicative
import Options.Applicative.Common (runParser)
import System.Directory (createDirectoryIfMissing)
import System.IO
import System.IO.Error
import Widgets.LogView.Types (CopyMethod (..))
import Data.Char (ord)
import Text.Read (readMaybe)

viewerArgs :: Parser AppArguments
viewerArgs = do
  input <-
    let i =
          metavar "FILE"
            <> help
              "The format can be derived from the file format. \
              \If the file format is .csv, the output will be csv, otherwise it will be json."
     in fromMaybe Stdin <$> optional (File <$> argument str i)

  format <-
    let i =
          long "format"
            <> short 'f'
            <> metavar "FORMAT"
            <> help
              "Supported formats: jsonl, csv. In the case of csv, \
              \the first line is considered the header."
     in optional . option formatReader $ i

  defaultField <-
    let i = long "default-field" <> short 'd' <> metavar "FIELD"
     in optional (strOption i)

  configPath <-
    let i = long "config" <> short 'c' <> metavar "CONFIG"
     in optional (strOption i)

  ignoreConfig <-
    let i =
          long "ignore-config"
            <> short 'i'
            <> help "Makes monalog ignore config, expected values: global, local, all"
        reader = maybeReader \case
          "global" -> Just Global
          "local" -> Just Local
          "all" -> Just All
          _ -> Nothing
     in optional . option reader $ i

  prefix <-
    let i = long "prefix" <> short 'p' <> help "Expects prefix before every line. Supported values: empty, kube-tm"
        reader = maybeReader \case
          "empty" -> Just Empty
          "kube-tm" -> Just KubeTm
          _ -> Nothing
     in optional . option reader $ i

  csvDelimiter <- 
    let i = long "delimiter" <> short 'd' <> help "comma, semicolon, or any 8-bit number"
        comma = fromIntegral $ ord ','
        semicolon = fromIntegral $ ord ';' 
        reader = maybeReader \case
          "comma" -> Just (fromIntegral $ ord ',')
          "semicolon" -> Just (fromIntegral $ ord ';')
          str | Just n <- readMaybe str -> Just n 
          _ -> Nothing
     in fmap (fromMaybe semicolon). optional . option reader $ i

  pure $ AppArguments{..}

data ConfigCmd
  = GetLocalConfigPath
  | GetGlobalConfigPath
  | CreateLocalConfig Bool
  | CreateGlobalConfig Bool

configArgs :: Parser ConfigCmd
configArgs = hsubparser do
  command
    "config"
    ( let pathCommand =
            command
              "path"
              ( let getLocalPath = command "local" (info (pure GetLocalConfigPath) (fullDesc <> progDesc "Prints local config path"))
                    getGlobalPath = command "global" (info (pure GetGlobalConfigPath) (fullDesc <> progDesc "Prints global config path"))
                 in info (hsubparser (getLocalPath <> getGlobalPath)) (fullDesc <> progDesc "Find out config path")
              )
          createCommand =
            command
              "create"
              let globalLocal = flag CreateLocalConfig CreateGlobalConfig (long "global")
                  force = switch (long "force" <> short 'f')
                  desc = fullDesc <> progDesc "Creates new config with comments"
               in info (($) <$> globalLocal <*> force) desc

          desc = progDesc "Work with config"
       in info (hsubparser (pathCommand <> createCommand)) desc
    )

main :: IO ()
main =
  customExecParser
    (prefs showHelpOnError)
    ( info
        do (Left <$> configArgs <|> Right <$> viewerArgs) <**> helper
        do fullDesc
    )
    >>= either
      do
        \case
          GetLocalConfigPath -> putStrLn configName
          GetGlobalConfigPath -> globalConfigDirPath >>= putStrLn . (<> configName)
          CreateLocalConfig force -> createConfig force configName
          CreateGlobalConfig force -> globalConfigDirPath >>= createConfig force
      do app

formatReader :: ReadM Format
formatReader = maybeReader \case
  "json" -> Just Jsonl
  "csv" -> Just Csv
  _ -> Nothing

copyMethodReader = maybeReader \case
  "native" -> Just Native
  "osc52" -> Just Osc52
  _ -> Nothing

createConfig :: Bool -> FilePath -> IO ()
createConfig force path = do
  result <-
    dumpConfigTo
      path
      force
      AppConfig
        { defaultField = Last $ Just "message"
        , fields = Last $ Just ["message", "severity"]
        , format = Last $ Just Jsonl
        , copyMethod = Last $ Just Native
        , copyCommand = Last Nothing
        , prefix = Last Nothing
        }
  case result of
    Left (Config.FileExists path) -> putStrLn $ "Error saving a config: file " <> path <> " exists. Use --force to overwrite."
    Left (Config.UnexpectedError e) -> putStrLn $ "Error saving a config: " <> show e
    Right _ -> putStrLn $ "Config created at " <> path <> configName
