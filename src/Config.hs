module Config where

import Control.Exception (catch)
import Control.Monad ((>=>))
import Data.Aeson (withText)
import Data.ByteString qualified as Bytes
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeEither', prettyPrintParseException)
import Data.Yaml.Aeson (FromJSON (parseJSON))
import GHC.Generics (Generic)
import GHC.IO (throwIO)
import System (System (..), currentOs)
import System.Directory
import System.IO.Error
import Type.Event (FatalError (..))
import Widgets.LogView.Types (CopyMethod)

data Input = Stdin | File FilePath
data Format = Json | Csv

instance FromJSON Format where
  parseJSON = withText "Format" \case
    "csv" -> pure Csv
    "json" -> pure Json
    _ -> fail "Failed to parse format"

data ConfigType = Local | Global | All

data AppConfig = AppConfig
  { defaultField :: Last Text
  , format :: Last Format
  , fields :: Last [Text]
  , copyMethod :: Last CopyMethod
  , copyCommand :: Last String
  }
  deriving (Generic, FromJSON)

instance Semigroup AppConfig where
  a <> b =
    AppConfig
      { defaultField = a.defaultField <> b.defaultField
      , format = a.format <> b.format
      , fields = a.fields <> b.fields
      , copyMethod = a.copyMethod <> b.copyMethod
      , copyCommand = a.copyCommand <> b.copyCommand
      }

instance Monoid AppConfig where
  mempty = AppConfig empty empty empty empty empty
   where
    empty :: Last a
    empty = Last Nothing

loadConfig :: Maybe String -> Maybe ConfigType -> IO AppConfig
loadConfig userConfigPath = maybe
  do (<>) <$> loadGlobal <*> loadLocal
  do
    \case
      Local -> loadGlobal
      Global -> loadLocal
      All -> maybe
        do pure mempty
        do loadFromFile >=> maybe (throwFatalError "Config file not found") pure
        do userConfigPath
 where
  loadFromFile path = catch
    do
      Bytes.readFile path
        >>= either (throwFatalError . ((path <> ": ") <>) . prettyPrintParseException) (pure . Just)
          . decodeEither' @AppConfig
    do catchFileNotFound

  loadGlobal = globalPath >>= fmap (fromMaybe mempty) . loadFromFile . (<> configName)
  loadLocal = fromMaybe mempty <$> loadFromFile configName

  catchFileNotFound e =
    if isDoesNotExistErrorType (ioeGetErrorType e)
      then pure Nothing
      else throwIOError e

  throwIOError = throwFatalError . ioeGetErrorString

globalPath :: IO FilePath
globalPath = globalConfigDirPath <&> (<> configName)

configName :: FilePath
configName = "monalog.yaml"

throwFatalError :: String -> IO a
throwFatalError = throwIO . MkFatalError . Text.pack

globalConfigDirPath :: IO FilePath
globalConfigDirPath =
  getXdgDirectory XdgConfig ""
    <&> ( <>
            case currentOs of
              Windows -> "\\monalog\\"
              _ -> "/monalog/"
        )
