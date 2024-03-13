module Widgets.Query.Handler where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Lens
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Data.Text.Zipper qualified as Z
import Data.Text.Zipper.Generic.Words qualified as Z
import Graphics.Vty qualified as V
import Query.Parser (queryParser)
import Text.Megaparsec qualified as M
import Type.Name
import Widgets.Query.Types

queryWidgetHandleEvent ::
  Lens' s QueryWidget ->
  QueryWidgetCallbacks s ->
  QueryWidgetEvent ->
  B.EventM Name s ()
queryWidgetHandleEvent widgetState QueryWidgetCallbacks{..} = \case
  Key V.KLeft [V.MCtrl] -> moveCursor Z.moveWordLeft
  Key V.KRight [V.MCtrl] -> moveCursor Z.moveWordRight
  Key V.KLeft [] -> moveCursor Z.moveLeft
  Key V.KRight [] -> moveCursor Z.moveRight
  Key (V.KChar c) ms -> proxyToEditor (B.VtyEvent $ V.EvKey (V.KChar c) ms)
  Key V.KBS [] -> proxyToEditor (B.VtyEvent $ V.EvKey V.KBS [])
  Click n@QueryWidgetEditor loc ->
    let name = WidgetName (QueryWidgetName n)
     in proxyToEditor (B.MouseDown name V.BLeft [] loc)
  Click QueryWidgetErrorHint _ -> use (widgetState . #parseError) >>= maybe (pure ()) showError
  Key V.KEnter [] -> tryApplyFilter
  _ -> pure ()
 where
  moveCursor f = invalidatingCacheAction do
    B.zoom widgetState do
      #input . B.editContentsL %= f

  invalidatingCacheAction action = B.invalidateCacheEntry queryWidgetName >> action

  proxyToEditor e = invalidatingCacheAction do
    B.zoom (widgetState . #input) do
      B.handleEditorEvent e
    content <- getEditorContent
    let clearError = widgetState . #parseError .= Nothing
    if Text.null (Text.strip content)
      then clearError
      else case M.runParser queryParser "Query" content of
        Left err -> widgetState . #parseError .= Just (stringifyErr err)
        Right _ -> clearError

  getEditorContent = use $ widgetState . #input . to B.getEditContents . to Text.concat

  stringifyErr = Text.pack . M.errorBundlePretty

  tryApplyFilter = do
    editorContent <- getEditorContent
    if Text.null (Text.strip editorContent)
      then clearFilter
      else case M.runParser queryParser "Query" editorContent of
        Right query -> execFilter query
        Left (stringifyErr -> err) -> showError err
