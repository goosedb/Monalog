module Widgets.Fields.Handler where

import Brick qualified as B
import Config (DumpError (..), dumpConfigToLocal)
import Control.Lens
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Type.Field
import Type.Log
import Type.MaxWidth
import Type.Name
import Type.WidgetSize (WidgetSize (..))
import Widgets.Fields.Types
import Widgets.Scrollbar.Horizontal qualified as HScroll

fieldsWidgetHandleEvent ::
  Lens' s FieldsWidget ->
  FieldsWidgetCallbacks s ->
  FieldWidgetEvent ->
  B.EventM Name s ()
fieldsWidgetHandleEvent widgetState cb@FieldsWidgetCallbacks{..} = \case
  NewLog l -> handleNewLog widgetState cb l
  Click (FieldWidgetHScrollBar B.SBBar) loc ->
    holdMouse (mkName $ FieldWidgetHScrollBar B.SBBar) loc
  Click FieldWidgetBorder loc@(B.Location (c, _)) -> do
    B.Extent{extentUpperLeft = B.Location (lc, _)} <- fromJust <$> B.lookupExtent (mkName FieldWidgetItself)
    widgetState . #width .= Manual (c - lc)
    holdMouse (mkName FieldWidgetBorder) loc
  Move (FieldWidgetHScrollBar B.SBBar) prevLoc newLoc -> do
    HScroll.handleScroll prevLoc newLoc (mkName FieldWidgetViewport)
    B.invalidateCache
  Move FieldWidgetBorder (B.Location (prevC, _)) (B.Location (newC, _)) -> do
    widgetState . #width %= \case
      Manual w -> Manual $ max 10 $ w + (newC - prevC)
      Auto -> Auto
    B.invalidateCache
  Click FieldWidgetLayoutButton _ -> do
    widgetState . #layout %= \case
      Flatten -> Nested
      Nested -> Flatten
    B.invalidateCache
  Click FieldWidgetSaveConfig _ -> do
    config <- cb.getConfig
    result <- liftIO $ dumpConfigToLocal config
    cb.configSaved $ case result of
      Left (FileExists path) -> SaveErrorHappened $ "File " <> Text.pack path <> " already exists"
      Left (UnexpectedError e) -> SaveErrorHappened $ "Unexpected error: " <> e
      Right _ -> SavedSuccessfully
  Click (FieldWidgetField field) _ -> do
    fields <- use $ widgetState . #fields
    let isFieldUpdating k = case (field, k) of
          (Field selectedPath, Field currentPath) -> selectedPath `isPrefixOf` currentPath
          (a, b) -> a == b
    let fieldsToUpdate = filter
          do \(k, _) -> isFieldUpdating k
          do Map.toList fields
    let isAnyUpdatingFieldIsActive = any ((.isSelected) . snd) fieldsToUpdate
    let setToActive = not isAnyUpdatingFieldIsActive
    let updatedFields = Map.mapWithKey
          do \k fs@FieldState{..} -> if isFieldUpdating k then FieldState{isSelected = setToActive, ..} else fs
          do fields
    forM_ fieldsToUpdate \(f, FieldState{..}) ->
      (if setToActive then fieldSelected maxWidth else fieldUnselected) f
    B.invalidateCache
    widgetState . #fields .= updatedFields
  Scroll i -> B.vScrollBy (B.viewportScroll $ mkName FieldWidgetViewport) i
  AltScroll i -> B.hScrollBy (B.viewportScroll $ mkName FieldWidgetViewport) i
  CleanupFields -> do
    defaultFields <- use $ widgetState . #defaultFields
    widgetState . #fields .= defaultFields
    B.invalidateCache
  _ -> pure ()

handleNewLog :: Lens' s FieldsWidget -> FieldsWidgetCallbacks s -> Log -> B.EventM Name s ()
handleNewLog widgetState FieldsWidgetCallbacks{..} l = do
  let valueKeys = keys l.value
  fields <- use $ widgetState . #fields
  let updateField path mw fieldsMap = Map.alterF
        do
          maybe
            do (False, Just FieldState{isSelected = False, maxWidth = mw})
            do \FieldState{..} -> (mw > maxWidth, Just FieldState{maxWidth = max mw maxWidth, ..})
        do Field path
        do fieldsMap
  let (updatedPaths, updatedFields) = F.foldl'
        do
          \(updFields, fieldsMap) (path, mw) ->
            let (isFieldWidthUpdated, updatedFieldsMap) = updateField path mw fieldsMap
             in (if isFieldWidthUpdated then (path, mw) : updFields else updFields, updatedFieldsMap)
        do ([], fields)
        do valueKeys
  widgetState . #fields .= updatedFields
  unless (null updatedPaths) do
    fieldsChangedMaxSize (Map.fromList updatedPaths)

keys :: Value -> [(Path, MaxWidth)]
keys (Object o) = concatMap (\(k, v) -> map (first (k :)) $ keys v) (KeyMap.toList o)
keys (String txt) =
  [([], MaxWidth $ Text.length txt)]
keys (Bool _) =
  [([], MaxWidth 5)]
keys (Number n) =
  [([], MaxWidth $ F.length (show n))]
keys Null =
  [([], MaxWidth 4)]
keys (Array arr) =
  [
    ( []
    , MaxWidth
        . fromIntegral
        . Text.Lazy.length
        . Text.Lazy.decodeUtf8
        $ encode arr
    )
  ]
