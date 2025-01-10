module Config where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, try)
import Control.Lens (Lens', (%~))
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (Options (..), defaultOptions, genericToJSON, withText)
import Data.ByteString qualified as Bytes
import Data.Functor (void, (<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Last (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeEither', encodeFile, prettyPrintParseException)
import Data.Yaml.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import GHC.Generics (Generic)
import GHC.IO (throwIO)
import Query.Parser (fieldParser)
import Query.Pretty (fieldPretty)
import System (System (..), currentOs)
import System.Directory
import System.FilePath
import System.IO.Error
import Text.Megaparsec (runParser)
import Type.Event (FatalError (..))
import Type.Field
import Type.LogViewPosition
import Widgets.Fields.Types (FieldsViewLayout)
import Widgets.LogView.Types (CopyMethod)

data Input = Stdin | Files (NonEmpty FilePath)
  deriving (Show)

data Format = Jsonl | Csv deriving (Show)

data Prefix = KubeTm
  deriving (Show)

instance FromJSON Prefix where
  parseJSON = withText "Prefix" \case
    "kube-tm" -> pure KubeTm
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

data ConfigType = Local | Global | All
  deriving (Show)

data AppConfig = AppConfig
  { defaultField :: Last Text
  , format :: Last Format
  , fieldsLayout :: Last FieldsViewLayout
  , logViewSide :: Last LogViewPosition
  , textWrap :: Last Bool
  , columns :: Last [JsonField]
  , copyMethod :: Last CopyMethod
  , copyCommand :: Last String
  , prefix :: Last Prefix
  }
  deriving (Generic, FromJSON)

instance ToJSON AppConfig where
  toJSON = genericToJSON defaultOptions{omitNothingFields = True}

newtype JsonField = JsonField Field

instance ToJSON JsonField where
  toJSON (JsonField f) = String $ fieldPretty f

instance FromJSON JsonField where
  parseJSON = withText "Field" (either (const $ fail "Failed to parse field") (pure . JsonField) . runParser fieldParser "")

instance Semigroup AppConfig where
  a <> b =
    AppConfig
      { defaultField = a.defaultField <> b.defaultField
      , format = a.format <> b.format
      , columns = a.columns <> b.columns
      , textWrap = a.textWrap <> b.textWrap
      , fieldsLayout = a.fieldsLayout <> b.fieldsLayout
      , logViewSide = a.logViewSide <> b.logViewSide
      , copyMethod = a.copyMethod <> b.copyMethod
      , copyCommand = a.copyCommand <> b.copyCommand
      , prefix = a.prefix <> b.prefix
      }

instance Monoid AppConfig where
  mempty = AppConfig empty empty empty empty empty empty empty empty empty
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

loadFromFile :: FilePath -> IO (Maybe AppConfig)
loadFromFile path = catch
  do
    Bytes.readFile path
      >>= either (throwFatalError . ((path <> ": ") <>) . prettyPrintParseException) (pure . Just)
        . decodeEither' @AppConfig
  do catchFileNotFound

catchFileNotFound :: IOError -> IO (Maybe a)
catchFileNotFound e =
  if isDoesNotExistErrorType (ioeGetErrorType e)
    then pure Nothing
    else throwIOError e
 where
  throwIOError = throwFatalError . ioeGetErrorString

loadGlobal :: IO AppConfig
loadGlobal = globalPath >>= fmap (fromMaybe mempty) . loadFromFile

loadLocal :: IO AppConfig
loadLocal = fromMaybe mempty <$> loadFromFile configName

data DumpError = FileExists FilePath | UnexpectedError Text

dumpErrorToText :: DumpError -> Text
dumpErrorToText = \case
  FileExists fp -> Text.pack $ "File already exists: " <> fp
  UnexpectedError t -> t

-- | Dumps given config to local file path as yaml overriding existing
dumpConfigToLocal :: AppConfig -> IO (Either DumpError ())
dumpConfigToLocal = dumpConfigTo configName True

dumpConfigToGlobal :: AppConfig -> IO (Either DumpError ())
dumpConfigToGlobal cfg = globalPath >>= \p -> dumpConfigTo p True cfg

updateGlobalConfig :: (MonadIO m) => Lens' AppConfig (Last a) -> (Maybe a -> a) -> m (Either DumpError ())
updateGlobalConfig l f = liftIO $ loadGlobal >>= dumpConfigToGlobal . (l %~ (Last . Just . f . getLast))

updateLocalConfig :: (MonadIO m) => Lens' AppConfig (Last a) -> (Maybe a -> a) -> m (Either DumpError ())
updateLocalConfig l f = liftIO $ loadLocal >>= dumpConfigToLocal . (l %~ (Last . Just . f . getLast))

updateGlobalConfigAsync :: (MonadIO m) => Lens' AppConfig (Last a) -> (Maybe a -> a) -> m ()
updateGlobalConfigAsync l = liftIO . void . forkIO . void . updateGlobalConfig l

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
