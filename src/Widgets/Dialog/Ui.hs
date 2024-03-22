module Widgets.Dialog.Ui where

import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as B
import Data.List (intersperse)
import Data.Text qualified as Text
import Graphics.Vty qualified as V
import Type.Name
import Widgets.Dialog.Types

dialogWidgetDraw :: DialogWidget s -> B.Widget Name
dialogWidgetDraw DialogWidget{..} = B.centerLayer $ B.border $ B.padAll 1 do
  B.hLimit (maximum . map Text.length . Text.lines $ dialogMessage) do
    B.vBox
      [ B.txt dialogMessage
      , B.txt " "
      , B.hCenter $
          B.hBox $
            intersperse (B.txt " ") $
              zipWith
                ( \i (k, _) ->
                    let highlight =
                          if i == selected
                            then
                              B.modifyDefAttr
                                (\V.Attr{..} -> V.Attr{attrStyle = V.SetTo V.reverseVideo, ..})
                            else id
                     in highlight $ B.clickable (mkName $ DialogButton k) $ B.txt $ "[" <> k <> "]"
                )
                [0 ..]
                actions
      ]
