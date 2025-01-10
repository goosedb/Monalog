module Widgets.Fields.Handler where

import Brick qualified as B
import Column (Column (..))
import Config (updateGlobalConfigAsync)
import Control.Lens
import Control.Monad (forM_, unless)
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Type.Event qualified as E
import Type.Field
import Type.Name
import Type.WidgetSize (WidgetSize (..))
import Widgets.Fields.Types
import Widgets.Scrollbar.Horizontal qualified as HScroll
import Widgets.Scrollbar.Vertical qualified as VScroll
import Prelude hiding (log)

fieldsWidgetHandleEvent ::
  Lens' s FieldsWidget ->
  FieldsWidgetCallbacks s ->
  FieldWidgetEvent ->
  B.EventM Name s ()
fieldsWidgetHandleEvent widgetState cb@FieldsWidgetCallbacks{..} = \case
  NewLog l -> handleNewLog widgetState cb l
  Click (FieldWidgetHScrollBar B.SBBar) loc -> holdMouse (mkName $ FieldWidgetHScrollBar B.SBBar) loc
  Click (FieldWidgetVScrollBar B.SBBar) loc -> holdMouse (mkName $ FieldWidgetVScrollBar B.SBBar) loc
  Click FieldWidgetBorder loc@(B.Location (c, _)) -> do
    B.Extent{extentUpperLeft = B.Location (lc, _)} <- fromJust <$> B.lookupExtent (mkName FieldWidgetItself)
    widgetState . #width .= Manual (c - lc)
    holdMouse (mkName FieldWidgetBorder) loc
  Move (FieldWidgetHScrollBar B.SBBar) prevLoc newLoc -> do
    HScroll.handleScroll prevLoc newLoc (mkName FieldWidgetViewport)
    B.invalidateCache
  Move (FieldWidgetVScrollBar B.SBBar) prevLoc newLoc -> do
    VScroll.handleScroll prevLoc newLoc (mkName FieldWidgetViewport)
    B.invalidateCache
  Move FieldWidgetBorder (B.Location (prevC, _)) (B.Location (newC, _)) -> do
    widgetState . #width %= \case
      Manual w -> Manual $ max 10 $ w + (newC - prevC)
      Auto -> Auto
    B.invalidateCache
  Click FieldWidgetLayoutButton _ -> do
    new <- uses (widgetState . #layout) \case
      Flatten -> Nested
      Nested -> Flatten
    widgetState . #layout .= new
    updateGlobalConfigAsync #fieldsLayout (const new)
    B.invalidateCache
  Click FieldWidgetSaveConfig _ -> cb.saveFieldsSet
  Click (FieldWidgetField field) _ -> selectField widgetState cb field
  SelectField field -> selectField widgetState cb field
  Scroll i -> B.vScrollBy (B.viewportScroll $ mkName FieldWidgetViewport) i
  AltScroll i -> B.hScrollBy (B.viewportScroll $ mkName FieldWidgetViewport) i
  CleanupFields -> do
    defaultFields <- use $ widgetState . #defaultFields
    widgetState . #fields .= defaultFields
    B.invalidateCache
  _ -> pure ()

data FieldState = NotUpdated | Updated | Created
  deriving (Eq)

selectField :: Lens' s FieldsWidget -> FieldsWidgetCallbacks s -> Field -> B.EventM Name s ()
selectField widgetState FieldsWidgetCallbacks{..} field = do
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

handleNewLog :: Lens' s FieldsWidget -> FieldsWidgetCallbacks s -> E.NewLog -> B.EventM Name s ()
handleNewLog widgetState FieldsWidgetCallbacks{..} E.NewLog{..} = do
  fields <- use $ widgetState . #fields
  let updateField path mw fieldsMap = Map.alterF
        do
          maybe
            do (Created, Just FieldState{isSelected = False, maxWidth = mw})
            do \FieldState{..} -> (if mw > maxWidth then Updated else NotUpdated, Just FieldState{maxWidth = max mw maxWidth, ..})
        do Field path
        do fieldsMap
  let (updatedPaths, createdPaths, updatedFields) = F.foldl'
        do
          \(updFields, createdFields, fieldsMap) Column{..} ->
            let (fieldState, updatedFieldsMap) = updateField path width fieldsMap
             in ( if fieldState == Updated then (path, width) : updFields else updFields
                , if fieldState == Created then path : createdFields else createdFields
                , updatedFieldsMap
                )
        do ([], [], fields)
        do columns
  widgetState . #fields .= updatedFields
  unless (null updatedPaths) do
    fieldsChangedMaxSize (Map.fromList updatedPaths)
  unless (null createdPaths) do
    fieldsCreated createdPaths
