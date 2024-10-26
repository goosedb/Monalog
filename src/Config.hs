module Config where

import Control.Exception (SomeException, catch, try)
import Control.Monad ((>=>))
import Data.Aeson (withText)
import Data.ByteString qualified as Bytes
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeEither', encodeFile, prettyPrintParseException)
import Data.Yaml.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import GHC.Generics (Generic)
import GHC.IO (throwIO)
import System (System (..), currentOs)
import System.Directory
import System.FilePath
import System.IO.Error
import Type.Event (FatalError (..))
import Widgets.LogView.Types (CopyMethod)

data Input = Stdin | File FilePath
data Format = Jsonl | Csv
data Prefix = KubeTm | Empty

instance FromJSON Prefix where
  parseJSON = withText "Prefix" \case
    "kube-tm" -> pure KubeTm
    "empty" -> pure Empty
    _ -> fail "Failed to parse prefix"

instance FromJSON Format where
  parseJSON = withText "Format" \case
    "csv" -> pure Csv
    "jsonl" -> pure Jsonl
    _ -> fail "Failed to parse format"

instance ToJSON Format where
  toJSON =
    String . \case
      Csv -> "csv"
      Jsonl -> "jsonl"

instance ToJSON Prefix where
  toJSON =
    String . \case
      KubeTm -> "kube-tm"
      Empty -> "empty"

data ConfigType = Local | Global | All

data AppConfig = AppConfig
  { defaultField :: Last Text
  , format :: Last Format
  , fields :: Last [Text]
  , copyMethod :: Last CopyMethod
  , copyCommand :: Last String
  , prefix :: Last Prefix
  }
  deriving (Generic, FromJSON, ToJSON)

instance Semigroup AppConfig where
  a <> b =
    AppConfig
      { defaultField = a.defaultField <> b.defaultField
      , format = a.format <> b.format
      , fields = a.fields <> b.fields
      , copyMethod = a.copyMethod <> b.copyMethod
      , copyCommand = a.copyCommand <> b.copyCommand
      , prefix = a.prefix <> b.prefix
      }

instance Monoid AppConfig where
  mempty = AppConfig empty empty empty empty empty empty
   where
    empty :: Last a
    empty = Last Nothing

loadConfig :: Maybe FilePath -> Maybe ConfigType -> IO AppConfig
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

data DumpError = FileExists FilePath | UnexpectedError Text

-- | Dumps given config to local file path as yaml overriding existing
dumpConfigToLocal :: AppConfig -> IO (Either DumpError ())
dumpConfigToLocal = dumpConfigTo configName True

-- | Dumps given config to given file path as yaml
dumpConfigTo :: FilePath -> Bool -> AppConfig -> IO (Either DumpError ())
dumpConfigTo path force config = do
  result <- try @SomeException $ do
    createDirectoryIfMissing True $ takeDirectory path
    exists <- doesFileExist path
    if not force && exists
      then pure $ Left $ FileExists path
      else Right <$> encodeFile path config
  pure $ case result of
    Left ex -> Left $ UnexpectedError $ Text.pack $ show ex
    Right (Left e) -> Left e
    Right _ -> Right ()

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
