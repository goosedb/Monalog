module Widgets.Query.Handler where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Applicative (Alternative ((<|>)))
import Control.Lens (
  Bifunctor (bimap),
  Lens',
  use,
  uses,
  (%=),
  (.=),
 )
import Control.Lens qualified as L
import Control.Monad (guard)
import Data.Aeson (encode)
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as Lazy.Bytes
import Data.Generics.Labels ()
import Data.List (sort)
import Data.List.NonEmpty qualified as Nel
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Zipper qualified as Z
import Graphics.Vty qualified as V
import Query (Located (..), Pos (..), Query (..), Span (..), isIn)
import Query.Parser (queryParser)
import Text.Megaparsec qualified as M
import Type.Field (Path, textPath)
import Type.Name
import Widgets.Editor (emptyEditor, handleEditorEvent)
import Widgets.Query.Types
import Prelude hiding (span)

queryWidgetHandleEvent ::
  Lens' s QueryWidget ->
  QueryWidgetCallbacks s ->
  QueryWidgetEvent ->
  B.EventM Name s ()
queryWidgetHandleEvent widgetState QueryWidgetCallbacks{..} = \case
  Key V.KEnter [] -> do
    selectedHint >>= \case
      Just (span, path) -> useHint span path
      Nothing -> tryApplyFilter
  Key (V.KChar '\t') [] -> do
    widgetState . #hint %= fmap \Hint{..} -> Hint{selected = Just $ maybe 0 (flip mod (Nel.length completions) . succ) selected, ..}
  Key k mods -> invalidatingCacheAction do
    B.zoom (widgetState . #input) do
      handleEditorEvent k mods
    updateQueryState
  Click n@QueryWidgetEditor loc _ -> do
    let name = WidgetName (QueryWidgetName n)
    B.zoom (widgetState . #input) do
      B.handleEditorEvent (B.MouseDown name V.BLeft [] loc)
    updateQueryState
  Click QueryWidgetErrorHint _ _ -> use (widgetState . #parseError) >>= maybe (pure ()) showError
  Click QueryWidgetErrorClear _ _ -> do
    widgetState . #input .= emptyEditor (mkName QueryWidgetEditor)
    updateQueryState
    clearFilter
  NewFields paths -> do
    widgetState . #fields %= (sort . (paths <>))
  AddFilter k v -> do
    content <- Text.strip <$> getEditorContent
    let kv = Text.intercalate "." k <> " = " <> Text.decodeUtf8 (Lazy.Bytes.toStrict $ encode v)
    let andKV = " && " <> kv
    let withFilter = case content of
          "" -> kv
          _ -> case parseInput content of
            Left _ -> content
            Right q -> case q of
              Or _ _ -> "(" <> content <> ")" <> andKV
              _ -> content <> andKV
    widgetState . #input %= B.applyEdit \zipper ->
      let c = Z.cursorPosition zipper
       in Z.moveCursor c $ Z.textZipper [withFilter] (Just 1)
    tryApplyFilter
  _ -> pure ()
 where
  selectedHint = uses (widgetState . #hint) (>>= \Hint{..} -> (span,) . (completions Nel.!!) <$> selected)

  invalidatingCacheAction action = B.invalidateCacheEntry queryWidgetName >> action

  useHint Span{..} path = do
    let textToPaste = Text.drop (to.column - from.column) $ textPath path
    widgetState . #input %= B.applyEdit (Z.insertChar ' ' . Z.insertMany textToPaste)
    clearHint

  clearHint = widgetState . #hint .= Nothing

  updateQueryState = do
    QueryWidget{..} <- use widgetState
    content <- getEditorContent
    cursor <- getEditorCursor
    let clearError = widgetState . #parseError .= Nothing
    if Text.null (Text.strip content)
      then clearError >> clearHint
      else case parseInput content of
        Left err -> do
          widgetState . #parseError .= Just (stringifyErr err)
          clearHint
        Right query ->
          clearError
            >> let completion = \case
                    Path Located{..} -> do
                      guard (cursor `isIn` span)
                      completions <- Nel.nonEmpty (filter (value `pathIsPrefixOf`) fields)
                      pure $ Hint{span, selected = Nothing, completions}
                    And a b -> completion a <|> completion b
                    Not a -> completion a
                    Or a b -> completion a <|> completion b
                    Eq a b -> completion a <|> completion b
                    Gt a b -> completion a <|> completion b
                    Like a b -> completion a <|> completion b
                    In a b -> completion a <|> completion b
                    Value _ -> Nothing
                in widgetState . #hint .= completion query

  getEditorContent = use $ widgetState . #input . L.to B.getEditContents . L.to Text.concat
  getEditorCursor = use $ widgetState . #input . L.to B.getCursorPosition . L.to (uncurry Pos . bimap succ succ)

  parseInput = M.runParser queryParser "Query"
  getParsedInput = parseInput <$> getEditorContent

  stringifyErr = Text.pack . M.errorBundlePretty

  tryApplyFilter = do
    editorContent <- getEditorContent
    if Text.null (Text.strip editorContent)
      then clearFilter
      else case M.runParser queryParser "Query" editorContent of
        Right query -> execFilter query
        Left (stringifyErr -> err) -> showError err

pathIsPrefixOf :: Path -> Path -> Bool
pathIsPrefixOf [] [] = False
pathIsPrefixOf [a] (b : _) = Text.isPrefixOf (Key.toText a) (Key.toText b)
pathIsPrefixOf (a : as) (b : bs) = a == b && pathIsPrefixOf as bs
pathIsPrefixOf _ _ = False
