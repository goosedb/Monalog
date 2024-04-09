{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
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
              "Supported formats: json, csv. In the case of csv, \
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
            <> help "Makes monalog ignore config"
            <> help "Expected values: global, local, all"
        reader = maybeReader \case
          "global" -> Just Global
          "local" -> Just Local
          "all" -> Just All
          _ -> Nothing
     in optional . option reader $ i

  prefix <- 
    let i = long "prefix" <> short 'p'
        reader = maybeReader \case
          "empty" -> Just Empty
          "kube-tm" -> Just KubeTm
          _ -> Nothing
    in optional . option reader $ i

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
          CreateLocalConfig force -> createConfig force ""
          CreateGlobalConfig force -> globalConfigDirPath >>= createConfig force
      do app

formatReader :: ReadM Format
formatReader = maybeReader \case
  "json" -> Just Json
  "csv" -> Just Csv
  _ -> Nothing

copyMethodReader = maybeReader \case
  "native" -> Just Native
  "osc52" -> Just Osc52
  _ -> Nothing

createConfig :: Bool -> FilePath -> IO ()
createConfig force path = do
  let filePath = path <> configName
  doesConfigExist <- catch
    do withFile filePath ReadMode (const (pure True))
    do pure . not . isDoesNotExistErrorType . ioeGetErrorType
  if not doesConfigExist || force
    then do
      createDirectoryIfMissing True path
      bool appendFile writeFile doesConfigExist filePath $
        unlines
          [ "# Field in which invalid json will be written. Useful only for json format."
          , "defaultField: \"message\""
          , ""
          , "# json / csv"
          , "format: json"
          , ""
          , "# Fields selected by default"
          , "fields:"
          , "  - message"
          , "  - severity"
          , ""
          , "# osc52 / native"
          , "copyMethod: native"
          , ""
          , "# Command to copy with `native` copy method. Must take copied string from stdin"
          , "copyCommand: null"
          ]
      putStrLn $ "Config created at " <> filePath
    else putStrLn "Config already exists. To overwrite use --force"
