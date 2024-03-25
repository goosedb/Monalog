{-# LANGUAGE DataKinds #-}

module Widgets.LogView.Handler where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Lens
import Control.Monad.IO.Class (MonadIO (..))
import Copy.Native qualified as Native
import Copy.Osc52 qualified as Osc52
import Data.Aeson (encode)
import Data.Aeson.Types (Value (..))
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Generics.Labels ()
import Data.JSONPath.Execute qualified as PE
import Data.JSONPath.Parser qualified as JP
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import GHC.Exts (IsList (..))
import Graphics.Vty qualified as V
import Text.Megaparsec qualified as M
import Type.Log (Log (..))
import Type.LogViewPosition (LogViewPosition (..))
import Type.Name (LogViewWidgetName (..), Name)
import Type.WidgetSize (WidgetSize (..))
import Widgets.Editor (handleEditorEvent)
import Widgets.LogView.Types

logViewWidgetHandleEvent :: LogViewPosition -> Lens' s LogViewWidget -> LogViewWidgetCallbacks s -> LogViewWidgetEvent -> B.EventM Name s ()
logViewWidgetHandleEvent logViewPosition widgetState callbacks = \case
  LogSelected l -> do
    B.invalidateCacheEntry (mkName LogViewWidgetItself)
    B.vScrollToBeginning (B.viewportScroll (mkName LogViewWidgetViewport))
    widgetState . #selectedLog ?= l
    updateFilteredValue
  Scroll i -> do
    B.vScrollBy (B.viewportScroll (mkName LogViewWidgetViewport)) i
  Move LogViewWidgetBorder (B.Location prevLoc) (B.Location newLoc) -> do
    case logViewPosition of
      LogViewPositionBottom ->
        widgetState . #height %= \case
          Manual i -> Manual $ max 3 $ i + (snd prevLoc - snd newLoc)
          Auto -> Auto
      LogViewPositionRight ->
        widgetState . #width %= \case
          Manual i -> Manual $ max 10 $ i + (fst prevLoc - fst newLoc)
          Auto -> Auto
    B.invalidateCache
  Move{} -> pure ()
  Click b loc _ -> case b of
    LogViewWidgetBorder -> do
      B.Extent{extentSize = (c, r)} <-
        fromJust <$> B.lookupExtent (mkName LogViewWidgetItself)
      case logViewPosition of
        LogViewPositionBottom -> widgetState . #height .= Manual (r + 2)
        LogViewPositionRight -> widgetState . #width .= Manual (c + 2)
      B.invalidateCache
    LogViewWidgetJsonpathCheckbox -> do
      widgetState . #showJsonpath %= not
      B.invalidateCacheEntry (mkName LogViewWidgetItself)
    LogViewWidgetJsonpathEditor -> do
      widgetState . #showJsonpath .= True
      B.zoom (widgetState . #jsonPathEditor) do
        B.handleEditorEvent (B.MouseDown (mkName LogViewWidgetJsonpathEditor) V.BLeft [] loc)
    LogViewWidgetCopyLog -> do
      LogViewWidget{..} <- use widgetState
      if showJsonpath
        then either (const $ pure ()) copy jsonpathFilteredValue
        else case selectedLog of
          Nothing -> pure ()
          Just Log{..} -> copy value
    LogViewWidgetCopyMethod -> do
      widgetState . #copyMethod %= \case
        Osc52 -> Native
        Native -> Osc52
      B.invalidateCacheEntry (mkName LogViewWidgetItself)
    _ -> pure ()
  Key k mods -> do
    B.zoom (widgetState . #jsonPathEditor) do
      handleEditorEvent k mods

    updateFilteredValue

    B.invalidateCacheEntry (mkName LogViewWidgetItself)
 where
  copy val = do
    cm <- use $ widgetState . #copyMethod
    liftIO case cm of
      Osc52 -> Osc52.copy (encode val)
      Native -> Native.copy (encode val)
    callbacks.copied
  stringifyErr = Text.pack . M.errorBundlePretty
  updateFilteredValue = do
    LogViewWidget{..} <- use widgetState

    case selectedLog of
      Nothing -> pure ()
      Just Log{..} -> do
        let jsonpathSource =
              Text.strip $ Text.concat (B.getEditContents jsonPathEditor)
            parsedPath = M.parse (JP.jsonPath M.eof) "jsonpath" jsonpathSource
        widgetState . #jsonpathFilteredValue .= bimap
          do stringifyErr
          do
            (\case [x] -> x; a -> Array $ fromList a)
              . ($ value)
              . PE.executeJSONPath
          do parsedPath
