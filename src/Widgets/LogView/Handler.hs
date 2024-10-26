{-# LANGUAGE DataKinds #-}

module Widgets.LogView.Handler where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Conduit (MonadIO (..))
import Control.Lens
import Control.Monad (when)
import Copy.Native qualified as Native
import Copy.Osc52 qualified as Osc52
import Data.Aeson (encode)
import Data.Aeson.Types (Value (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Generics.Labels ()
import Data.JSONPath.Execute qualified as PE
import Data.JSONPath.Parser qualified as JP
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Text qualified as Text
import GHC.Exts (IsList (..))
import Graphics.Vty qualified as V
import Text.Megaparsec qualified as M
import Type.LogViewPosition (LogViewPosition (..))
import Type.Name (LogViewWidgetName (..), Name)
import Type.WidgetSize (WidgetSize (..))
import Widgets.Editor (handleEditorEvent)
import Widgets.LogView.Tokenize (Token (..), TokenKind (Normal))
import Widgets.LogView.Types (
  CopyMethod (Native, Osc52),
  LogViewWidget (..),
  LogViewWidgetCallbacks (copied, copyError),
  LogViewWidgetEvent (..),
  LogViewWidgetSettings (..),
  TokenizedValue (TokenizedValue, tokens, value),
  generateCache,
  mkName,
 )
import Data.Function (on)

logViewWidgetHandleEvent :: LogViewPosition -> Lens' s LogViewWidget -> LogViewWidgetCallbacks s -> LogViewWidgetEvent -> B.EventM Name s ()
logViewWidgetHandleEvent logViewPosition widgetState callbacks = \case
  LogSelected -> B.invalidateCacheEntry (mkName LogViewWidgetContent)
  Scroll i -> scroll i
  Move LogViewWidgetBorder (B.Location prevLoc) (B.Location newLoc) -> do
    case logViewPosition of
      LogViewPositionBottom ->
        widgetState . #settings . #height %= \case
          Manual i -> Manual $ max 3 $ i + (snd prevLoc - snd newLoc)
          Auto -> Auto
      LogViewPositionRight ->
        widgetState . #settings . #width %= \case
          Manual i -> Manual $ max 10 $ i + (fst prevLoc - fst newLoc)
          Auto -> Auto
    updateCache >> B.invalidateCache
  Move LogViewWidgetScrollBar (B.Location prevLoc) (B.Location newLoc) -> do
    (_, fromIntegral @_ @Double -> r) <- contentExtent
    contLen <- uses (widgetState . #cache) (maybe 0 Seq.length)
    scroll (round $ fromIntegral (snd newLoc - snd prevLoc) / r * fromIntegral contLen)
    B.invalidateCache
  Move{} -> pure ()
  Click b loc _ -> case b of
    LogViewWidgetBorder -> do
      B.Extent{extentSize = (c, r)} <-
        fromJust <$> B.lookupExtent (mkName LogViewWidgetItself)
      case logViewPosition of
        LogViewPositionBottom -> widgetState . #settings . #height .= Manual (r + 2)
        LogViewPositionRight -> widgetState . #settings . #width .= Manual (c + 2)
      B.invalidateCache >> updateCache
    LogViewWidgetJsonpathCheckbox -> do
      widgetState . #settings . #showJsonpath %= not
      B.invalidateCacheEntry (mkName LogViewWidgetContent) >> updateCache
    LogViewWidgetJsonpathEditor -> do
      widgetState . #settings . #showJsonpath .= True
      B.zoom (widgetState . #settings . #jsonPathEditor) do
        B.handleEditorEvent (B.MouseDown (mkName LogViewWidgetJsonpathEditor) V.BLeft [] loc)
      updateFilter >> updateCache
    LogViewWidgetCopyLog -> do
      LogViewWidget{..} <- use widgetState
      let LogViewWidgetSettings {..} = settings
      if showJsonpath
        then either (const $ pure ()) (copy . extactValue selectedLog.value) jsonpathFilter
        else copy selectedLog.value
    LogViewWidgetCopyMethod -> do
      widgetState . #settings . #copyMethod %= \case
        Osc52 -> Native
        Native -> Osc52
      B.invalidateCacheEntry (mkName LogViewWidgetContent)
    LogViewWidgetWordWrap -> do
      let d = fromIntegral @_ @Double
      widgetState . #settings . #textWrap %= not
      offsetPercentage <- do
        LogViewWidget {..} <- use widgetState
        pure $ ((/) `on` d) offset (maybe 1 Seq.length cache)
      B.invalidateCacheEntry (mkName LogViewWidgetContent) >> updateCache
      newLength <- do
        LogViewWidget {..} <- use widgetState
        pure $ maybe 1 (d . Seq.length) cache
      widgetState . #offset .= round (newLength * offsetPercentage)
    LogViewWidgetCopyKey path -> do
      LogViewWidget{..} <- use widgetState
      let rawPath = Text.concat path
      let valueToCopy = do
            parsedPath <- either (const Nothing) Just $ parseJsonPath rawPath
            let TokenizedValue{..} = selectedLog
            pure $ extactValue value parsedPath
      maybe (pure ()) copy valueToCopy
    _ -> pure ()
  Key k mods -> do
    B.zoom (widgetState . #settings . #jsonPathEditor) do
      handleEditorEvent k mods
    updateFilter
    B.invalidateCacheEntry (mkName LogViewWidgetContent)
 where
  contentExtent = do
    B.Extent{extentSize = (c, r)} <-
      fromJust <$> B.lookupExtent (mkName LogViewWidgetContent)
    pure (c, r)

  updateCacheIfNeeded = do
    use (widgetState . #cache) >>= \case
      Just _ -> pure ()
      Nothing -> updateCache

  updateCache = do
    (c, _) <- contentExtent
    liftIO $ appendFile "log.log" (show c <> "\n")
    LogViewWidget{..} <- use widgetState
    let LogViewWidgetSettings{..} = settings
    let newCache =
          either
            do Seq.fromList . map (\t -> [Token [] t Normal])
            do id
            do second Seq.fromList $ generateCache (if textWrap then Just c else Nothing) selectedLog jsonpathFilter showJsonpath
    widgetState . #cache ?= newCache

  scroll i = do
    updateCacheIfNeeded
    uses widgetState (.cache) >>= \case
      Nothing -> pure ()
      Just cache -> do
        currOffset <- use (widgetState . #offset)
        let contLen = Seq.length cache
        let newOffset = max 0 . min (contLen - 1) $ (currOffset + i)
        when (newOffset /= currOffset) do
          B.invalidateCacheEntry (mkName LogViewWidgetContent)
          widgetState . #offset .= newOffset

  extactValue value =
    (\case [x] -> x; a -> Array $ fromList a)
      . ($ value)
      . PE.executeJSONPath

  parseJsonPath = M.parse (JP.jsonPath M.eof) "jsonpath"

  copy val = do
    cm <- use $ widgetState . #settings . #copyMethod
    cmd <- use $ widgetState . #settings . #nativeCopyCmd
    case cm of
      Osc52 -> Osc52.copy (encode val)
      Native -> Native.copy cmd (encode val) >>= either callbacks.copyError pure
    callbacks.copied
  stringifyErr = Text.pack . M.errorBundlePretty

  updateFilter = do
    LogViewWidget{..} <- use widgetState
    let jsonpathSource =
          Text.strip $ Text.concat (B.getEditContents settings.jsonPathEditor)
        parsedPath = parseJsonPath jsonpathSource
    let jpfilter = first stringifyErr parsedPath
    widgetState . #settings . #jsonpathFilter .= jpfilter
