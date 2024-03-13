module Widgets.Error.Ui where

import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as B
import Data.Text qualified as Text
import Type.Name
import Widgets.Error.Types

errorWidgetDraw :: ErrorWidget -> B.Widget Name
errorWidgetDraw ErrorWidget{..} = B.centerLayer $ B.borderWithLabel (B.txt "[Error]") $ B.padAll 1 do
  B.hLimit (maximum . map Text.length . Text.lines $ errorMessage) do
    B.vBox
      [ B.txt errorMessage
      , B.txt " "
      , B.hCenter $ B.clickable (mkName ErrorWidgetOk) $ B.txt "[Ok]"
      ]
