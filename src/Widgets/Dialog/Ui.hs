module Widgets.Dialog.Ui where

import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as B
import Control.Lens ((.~))
import Data.Generics.Labels ()
import Data.List (intersperse)
import Data.Text qualified as Text
import Graphics.Vty qualified as V
import Type.Name
import Widgets.Dialog.Types

dialogWidgetDraw :: DialogWidget s -> B.Widget Name
dialogWidgetDraw DialogWidget{..} = B.centerLayer $ B.borderWithLabel widgetTitle $ B.padAll 1 do
  B.hLimit (maximum . map Text.length . Text.lines $ dialogMessage) do
    B.vBox
      [ B.txt dialogMessage
      , B.txt " "
      , B.hCenter
          . B.hBox
          . intersperse (B.txt " ")
          . zipWith drawButtons [0 ..]
          $ actions
      ]
 where
  drawButtons i (k, _) =
    let highlight = if i == selected then B.modifyDefAttr (#attrStyle .~ V.SetTo V.reverseVideo) else id
     in highlight . B.clickable (mkName $ DialogButton k) . B.txt $ "[" <> k <> "]"

  widgetTitle = B.hBox $ map B.txt ["[", title, "]"]
