module Widgets.Fields.Ui where

import Brick qualified as B
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Type.Field
import Type.Name
import Widgets.Checkbox (drawCheckBox)
import Widgets.Fields.Types
import Widgets.Scrollbar.Vertical qualified as VScroll

fieldsWidgetDraw :: FieldsWidget -> B.Widget Name
fieldsWidgetDraw FieldsWidget{..} =
  B.hLimit hLimit
    . B.withVScrollBarRenderer
      VScroll.renderer
    . B.withVScrollBars B.OnRight
    . B.viewport (mkName FieldWidgetViewport) B.Vertical
    . B.cached (mkName FieldWidgetItself)
    . B.vLimit (Map.size fields)
    . B.vBox
    . map
      do
        \(field, FieldState{..}) -> do
          B.hBox
            [ drawCheckBox
                isSelected
                (mkName $ FieldWidgetField field)
                (drawLogsViewColumnHeaderTxt field)
            , B.txt " "
            ]
    $ Map.toList fields
 where
  hLimit =
    (+ 6)
      . maximum
      . map (Text.length . drawLogsViewColumnHeaderTxt . fst)
      $ Map.toList fields
