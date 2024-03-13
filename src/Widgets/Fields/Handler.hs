module Widgets.Fields.Handler where

import Brick qualified as B
import Control.Lens
import Control.Monad (unless)
import Data.Aeson (Value (..), encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Encoding qualified as Text.Lazy
import Type.Field
import Type.Log
import Type.MaxWidth
import Type.Name
import Widgets.Fields.Types

fieldsWidgetHandleEvent :: Lens' s FieldsWidget -> FieldsWidgetCallbacks s -> FieldWidgetEvent -> B.EventM Name s ()
fieldsWidgetHandleEvent widgetState cb@FieldsWidgetCallbacks{..} = \case
  NewLog l -> handleNewLog widgetState cb l
  Click (FieldWidgetField field) -> do
    fields <- use $ widgetState . #fields
    let ((isFieldSelected, width), updatedFields) = Map.alterF
          do
            maybe ((False, MaxWidth 0), Nothing) \FieldState{..} ->
              ((not isSelected, maxWidth), Just FieldState{isSelected = not isSelected, ..})
          do field
          do fields
    (if isFieldSelected then fieldSelected width else fieldUnselected) field
    B.invalidateCacheEntry (mkName FieldWidgetItself)
    B.invalidateCacheEntry (mkName $ FieldWidgetField field)
    widgetState . #fields .= updatedFields
  Scroll i -> B.vScrollBy (B.viewportScroll (mkName FieldWidgetViewport)) i
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
  unless (null updatedPaths) do
    fieldsChangedMaxSize (Map.fromList updatedPaths)
  widgetState . #fields .= updatedFields

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
