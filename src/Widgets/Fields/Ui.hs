module Widgets.Fields.Ui where

import Brick qualified as B
import Data.Aeson (Key, ToJSON)
import Data.Aeson.Key qualified as Key
import Data.Foldable qualified as F
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Type.Field
import Type.Name
import Widgets.Checkbox (drawCheckBox)
import Widgets.Fields.Types
import Widgets.Scrollbar.Horizontal qualified as HScroll
import Widgets.Scrollbar.Vertical qualified as VScroll

fieldsWidgetDraw :: FieldsWidget -> B.Widget Name
fieldsWidgetDraw FieldsWidget{..} =
  B.reportExtent (mkName FieldWidgetItself) $
    B.vBox
      [ B.clickable (mkName FieldWidgetLayoutButton) $ B.txt case layout of
          Flatten -> "[Flatten]"
          Nested -> "[Nested]"
      , B.withClickableHScrollBars (\e _ -> mkName (FieldWidgetHScrollBar e))
          . B.withHScrollBarRenderer
            HScroll.renderer
          . B.withHScrollBars B.OnBottom
          . B.withVScrollBarRenderer
            VScroll.renderer
          . B.withVScrollBars B.OnRight
          . B.viewport (mkName FieldWidgetViewport) B.Both
          . B.hLimit contentWidth
          $ B.vBox fieldsWidgets
      , B.clickable (mkName FieldWidgetSaveConfig) $ B.txt "[Save]"
      ]
 where
  recMap = buildRecMap fields
  drawRec path n m =
    Map.toList m & map \(k, entry) -> case entry of
      FieldRecEntry FieldState{..} -> [(isSelected, drawButton n path k isSelected)]
      FieldRecNest m' ->
        let parent = drawButton n path k anyChildrenActive
            anyChildrenActive = any fst children
            children = concat (drawRec (path <> [k]) (n + 2) m')
         in (anyChildrenActive, parent) : children
  fieldsWidgets = case layout of
    Nested ->
      let builtinFields = flip mapMaybe fieldsList \case
            (Field _, _) -> Nothing
            (f, FieldState{..}) ->
              Just $
                B.hBox
                  [ drawCheckBox
                      isSelected
                      (mkName $ FieldWidgetField f)
                      (drawLogsViewColumnHeaderTxt f)
                  , B.txt " "
                  ]
          otherFields = map snd $ concat $ drawRec [] (0 :: Int) recMap
       in builtinFields <> otherFields
    Flatten ->
      Map.toList fields & map \(f, FieldState{..}) ->
        drawCheckBox
          isSelected
          (mkName $ FieldWidgetField f)
          (drawLogsViewColumnHeaderTxt f)

  contentWidth =
    Map.toList fields
      & NE.nonEmpty
      & fmap (NE.map (Text.length . drawLogsViewColumnHeaderTxt . fst))
      & fmap (maximum . NE.toList)
      & maybe 6 (+ 6)

  drawButton n path k isSelected =
    B.hBox
      [ if n > 0 then B.str (replicate n ' ') else B.emptyWidget
      , drawCheckBox
          isSelected
          (mkName $ FieldWidgetField (Field (path <> [k])))
          ((if not (null path) then ("." <>) else id) (Key.toText k))
      ]

  fieldsList = Map.toList fields

data FieldRecEntry a
  = FieldRecEntry a
  | FieldRecNest (Map.Map Key (FieldRecEntry a))
  deriving (Show, Generic, ToJSON)

buildRecMap :: Map.Map Field a -> Map.Map Key (FieldRecEntry a)
buildRecMap = F.foldl' (\a (f, v) -> case f of Field ks -> insert ks v a; _ -> a) mempty . Map.toList

insert :: [Key] -> a -> Map.Map Key (FieldRecEntry a) -> Map.Map Key (FieldRecEntry a)
insert [] _ = id
insert [x] a = Map.insert x (FieldRecEntry a)
insert (x : xs) a = Map.alter
  do
    Just . maybe
      do FieldRecNest $ insert xs a mempty
      do
        \case
          FieldRecEntry _ -> FieldRecNest $ insert xs a mempty
          FieldRecNest m -> FieldRecNest (insert xs a m)
  do x
