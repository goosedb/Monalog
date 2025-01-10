module Widgets.Query.Handler where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Applicative (Alternative (empty, (<|>)), asum)
import Control.Lens (
  Bifunctor (bimap),
  Lens',
  use,
  uses,
  (%=),
  (%~),
  (.=),
 )
import Control.Lens qualified as L
import Control.Monad (guard)
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.List.NonEmpty qualified as Nel
import Data.Monoid (Endo (..))
import Data.Text qualified as Text
import Data.Text.Zipper qualified as Z
import Graphics.Vty qualified as V
import Query (Filter (..), Located (..), Pos (..), Query (..), Span (..), dummyLocated, dummySpan, isIn)
import Query.Parser (queryParser)
import Query.Pretty (pathPiecePretty, pathPretty)
import Query.Pretty qualified as Pretty
import Text.Megaparsec qualified as M
import Type.Field (Field (..), Path, timestampName)
import Type.Name
import Type.Sort (Sort (..))
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
    widgetState . #hint %= fmap \Hint{..} ->
      Hint
        { selected = Just $ maybe 0 (flip mod (Nel.length completions) . succ) selected
        , ..
        }
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
    widgetState . #fields %= (List.sort . (paths <>))
  AddFilter k v -> addFilter k v
  AddSort field -> addSort field
  _ -> pure ()
 where
  selectedHint = uses (widgetState . #hint) (>>= \Hint{..} -> (span,) . (completions Nel.!!) <$> selected)

  invalidatingCacheAction action = B.invalidateCacheEntry queryWidgetName >> action

  useHint Span{..} path = do
    let textToPaste = pathPretty path
    let deleteTyped = appEndo $ foldMap Endo $ replicate (to.column - from.column) Z.deletePrevChar
    widgetState . #input %= B.applyEdit (Z.insertChar ' ' . Z.insertMany textToPaste . deleteTyped . Z.moveCursor (0, to.column - 1))
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
        Right query -> clearError >> widgetState . #hint .= findCompletion query cursor fields

  getEditorContent = use $ widgetState . #input . L.to B.getEditContents . L.to Text.concat
  getEditorCursor = use $ widgetState . #input . L.to B.getCursorPosition . L.to (uncurry Pos . bimap succ succ)

  parseInput = M.runParser queryParser "Query"

  stringifyErr = Text.pack . M.errorBundlePretty

  tryApplyFilter = do
    editorContent <- getEditorContent
    if Text.null (Text.strip editorContent)
      then clearFilter
      else case parseInput editorContent of
        Right query -> execFilter query
        Left (stringifyErr -> err) -> showError err

  addFilter k v = do
    content <- Text.strip <$> getEditorContent
    let kv = Path (Located (map pathPiecePretty k) dummySpan) `Eq` Value v
        keyAlreadyExists (Path a `Eq` _) = a.value == k
        keyAlreadyExists (a `And` b) = keyAlreadyExists a || keyAlreadyExists b
        keyAlreadyExists _ = False

        deleteKey (Path a `Eq` _) | a.value == k = empty
        deleteKey (a `And` b) = case (deleteKey a, deleteKey b) of
          (Just x, Just y) -> Just (x `And` y)
          (Nothing, Just y) -> Just y
          (Just x, Nothing) -> Just x
          _ -> Nothing
        deleteKey a = Just a

        updateFilter f = if keyAlreadyExists f then deleteKey f else Just (f `And` kv)
    let updated = case content of
          "" -> Pretty.filterPretty kv
          _ -> case parseInput content of
            Left _ -> content
            Right q -> q & #filter %~ maybe (Just kv) updateFilter & Pretty.queryPretty
    applyFilterAndSort updated

  addSort field = do
    content <- Text.strip <$> getEditorContent
    let modifySort [] = [(Asc, dummyLocated field)]
        modifySort ((o, Located f span) : ss)
          | field == f = maybe ss ((: ss) . (,dummyLocated field)) $ case o of
              Asc -> Just Desc
              Desc -> Nothing
          | otherwise = (o, Located f span) : modifySort ss
    let updated = case content of
          "" -> Pretty.queryPretty (Query Nothing $ modifySort [])
          _ -> case parseInput content of
            Left _ -> content
            Right q -> q & #sort %~ modifySort & Pretty.queryPretty
    applyFilterAndSort updated

  applyFilterAndSort updated = do
    widgetState . #input %= B.applyEdit \zipper ->
      let c = Z.cursorPosition zipper
       in Z.moveCursor c $ Z.textZipper [updated] (Just 1)
    tryApplyFilter

findCompletion :: Query -> Pos -> [Path] -> Maybe Hint
findCompletion query cursor fields =
  let
    pathCompletion Located{..} = do
      guard (cursor `isIn` span)
      completions <- Nel.nonEmpty (Prelude.filter (value `pathIsPrefixOf`) fields)
      pure $ Hint{span, selected = Nothing, completions}
    queryCompletion = \case
      Path v -> pathCompletion v
      And a b -> queryCompletion a <|> queryCompletion b
      Not a -> queryCompletion a
      Or a b -> queryCompletion a <|> queryCompletion b
      Eq a b -> queryCompletion a <|> queryCompletion b
      Gt a b -> queryCompletion a <|> queryCompletion b
      Neq a b -> queryCompletion a <|> queryCompletion b
      Lt a b -> queryCompletion a <|> queryCompletion b
      Lte a b -> queryCompletion a <|> queryCompletion b
      Gte a b -> queryCompletion a <|> queryCompletion b
      Like a b -> queryCompletion a <|> queryCompletion b
      In a b -> queryCompletion a <|> queryCompletion b
      Value _ -> Nothing
      Array arr -> asum (map queryCompletion arr)
    sortCompletion = \case
      [] -> empty
      (_, Located{value = Timestamp}) : rest -> sortCompletion rest
      (_, Located{value = Field path, ..}) : rest -> case path of
        [p] | p `Text.isPrefixOf` timestampName -> pure Hint{span, selected = Nothing, completions = Nel.singleton [timestampName]}
        _ -> pathCompletion (Located{value = path, ..}) <|> sortCompletion rest
   in
    (query.filter >>= queryCompletion) <|> sortCompletion query.sort

pathIsPrefixOf :: Path -> Path -> Bool
pathIsPrefixOf [] [] = False
pathIsPrefixOf [a] (b : _) = Text.isPrefixOf a b
pathIsPrefixOf (a : as) (b : bs) = a == b && pathIsPrefixOf as bs
pathIsPrefixOf _ _ = False
