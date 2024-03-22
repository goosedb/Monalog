module Widgets.Query.Handler where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Lens
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Graphics.Vty qualified as V
import Query.Parser (queryParser)
import Text.Megaparsec qualified as M
import Type.Name
import Widgets.Editor (emptyEditor, handleEditorEvent)
import Widgets.Query.Types

queryWidgetHandleEvent ::
  Lens' s QueryWidget ->
  QueryWidgetCallbacks s ->
  QueryWidgetEvent ->
  B.EventM Name s ()
queryWidgetHandleEvent widgetState QueryWidgetCallbacks{..} = \case
  Key V.KEnter [] -> tryApplyFilter
  Key k mods -> invalidatingCacheAction do
    B.zoom (widgetState . #input) do
      handleEditorEvent k mods
    updateQueryState
  Click n@QueryWidgetEditor loc _ ->
    let name = WidgetName (QueryWidgetName n)
     in B.zoom (widgetState . #input) do
          B.handleEditorEvent (B.MouseDown name V.BLeft [] loc)
  Click QueryWidgetErrorHint _ _ -> use (widgetState . #parseError) >>= maybe (pure ()) showError
  Click QueryWidgetErrorClear _ _ -> do
    widgetState . #input .= emptyEditor (mkName QueryWidgetEditor)
    updateQueryState
    clearFilter
  _ -> pure ()
 where
  invalidatingCacheAction action = B.invalidateCacheEntry queryWidgetName >> action

  updateQueryState = do
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
