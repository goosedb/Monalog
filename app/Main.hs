{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import App
import Config
import Control.Exception (catch)
import Control.Lens (set, (&), (.~), (<&>))
import Control.Lens qualified as Data.ByteString
import Data.Aeson (encode, object, (.=))
import Data.Aeson qualified as A
import Data.Aeson qualified as J
import Data.Aeson.Key (fromString)
import Data.Bool (bool)
import Data.ByteString.Lazy qualified
import Data.Char (ord)
import Data.Csv (HasHeader (..), decode)
import Data.Foldable (Foldable (..))
import Data.Generics.Labels ()
import Data.List (nub)
import Data.List.NonEmpty (some1)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Debug.Trace (trace)
import GHC.IO.IOMode (IOMode (ReadMode))
import Options.Applicative
import Options.Applicative.Common (runParser)
import System.Directory (createDirectoryIfMissing)
import System.IO
import System.IO.Error
import Text.Read (readMaybe)
import Widgets.LogView.Types (CopyMethod (..))

viewerArgs :: Parser AppArguments
viewerArgs = do
  input <-
    let i =
          metavar "FILE"
            <> help
              "The format can be derived from the file format. \
              \If the file format is .csv, the output will be csv, otherwise it will be json."
     in fromMaybe Stdin <$> optional (Files <$> some1 (argument str i))

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
    let i = long "prefix" <> short 'p' <> help "Expects prefix before every (jsonl only for now) line. Supported values: empty, kube-tm"
        reader = maybeReader \case
          "empty" -> Nothing
          "kube-tm" -> Just KubeTm
          _ -> Nothing
     in optional . option reader $ i

  csvDelimiter <-
    let i = long "separator" <> short 's' <> help "comma, semicolon, any char or 8-bit number"
        comma = fromIntegral $ ord ','
        semicolon = fromIntegral $ ord ';'
        reader = maybeReader \case
          "comma" -> Just comma
          "semicolon" -> Just semicolon
          [c] -> Just (fromIntegral $ ord c)
          str | Just n <- readMaybe str -> Just n
          _ -> Nothing
     in fmap (fromMaybe comma) . optional . option reader $ i

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
        fullDesc
    )
    >>= either
      do
        \case
          GetLocalConfigPath -> putStrLn configName
          GetGlobalConfigPath -> globalConfigDirPath >>= putStrLn . (<> configName)
          CreateLocalConfig force -> createConfig force configName
          CreateGlobalConfig force -> globalConfigDirPath >>= createConfig force
      app

formatReader :: ReadM Format
formatReader = maybeReader \case
  "jsonl" -> Just Jsonl
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
      (mempty & #defaultField .~ Last (Just "message"))
  case result of
    Left (Config.FileExists path) -> putStrLn $ "Error saving a config: file " <> path <> " exists. Use --force to overwrite."
    Left (Config.UnexpectedError e) -> putStrLn $ "Error saving a config: " <> show e
    Right _ -> putStrLn $ "Config created at " <> path <> configName
