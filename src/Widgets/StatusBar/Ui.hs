module Widgets.StatusBar.Ui where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Generics.Labels ()
import Data.Text qualified as Text
import Type.LogViewPosition (LogViewPosition (..))
import Type.Name
import Widgets.Checkbox (drawCheckBox)
import Widgets.StatusBar.Types

statusBarWidgetDraw :: StatusBarWidget -> B.Widget Name
statusBarWidgetDraw StatusBarWidget{..} =
  B.hBox
    [ B.clickable (mkName StatusBarWidgetLogSide) $
        B.hBox
          [ B.txt "[Log view: "
          , B.txt case logViewPosition of
              LogViewPositionBottom -> "Bottom]"
              LogViewPositionRight -> "Right]"
          ]
    , B.txt " "
    , drawCheckBox followLogs (mkName StatusBarWidgetFollow) "Follow"
    , B.txt " "
    , B.padLeft B.Max $
        B.hBox
          [ case status of
              Idle -> B.emptyWidget
              JustCopied{} -> B.txt "Copied!"
              JustSavedConfig{} -> B.txt "Config saved!"
          , B.txt " "
          , B.hLimit (succ . sum . map Text.length . B.getEditContents $ topLineEditor) $
              B.renderEditor (B.hBox . map B.txt) isEditorActive topLineEditor
          , B.str "/"
          , B.str $ show totalLines
          ]
    ]
