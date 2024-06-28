{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Widgets.LogView.Ui where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Brick.Widgets.Skylighting qualified as B
import Control.Lens
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Data.Yaml qualified as Yaml
import Skylighting qualified as S
import Type.AvailableSpace
import Type.Log
import Type.Name
import Type.TBool (TBool, pattern Is)
import Widgets.Checkbox (drawCheckBox)
import Widgets.LogView.Types
import Widgets.Scrollbar.Vertical qualified as VScroll
import qualified Data.ByteString as Bytes

logViewWidgetDraw :: TBool "active" -> AvailableSpace -> LogViewWidget -> B.Widget Name
logViewWidgetDraw isActive availableSpace LogViewWidget{..} = case selectedLog of
  Nothing -> B.emptyWidget
  Just Log{..} ->
    B.reportExtent (mkName LogViewWidgetItself)
      . B.hLimit availableSpace.width
      . B.vBox
      $ [ B.hBox
            [ drawCheckBox showJsonpath (mkName LogViewWidgetJsonpathCheckbox) "Jsonpath"
            , B.txt ":"
            , B.padLeftRight 1 $
                B.hBox
                  [ B.renderEditor (B.hBox . map B.txt) (Is == isActive) jsonPathEditor
                  ]
            ]
        , B.withVScrollBarRenderer
            VScroll.renderer
            . B.withVScrollBars B.OnRight
            . B.viewport (mkName LogViewWidgetViewport) B.Vertical
            . B.cached (mkName LogViewWidgetItself)
            $ if showJsonpath
              then either
                do B.vBox . map B.txt . Text.lines
                do drawYaml
                do jsonpathFilteredValue
              else drawYaml value
        , B.hBox
            [ B.clickable (mkName LogViewWidgetCopyLog) $ B.txt "[Copy]"
            , B.padLeft B.Max
                . B.clickable (mkName LogViewWidgetCopyMethod)
                . B.txt
                $ case copyMethod of
                  Osc52 -> "[Osc52]"
                  Native -> "[Native]"
            ]
        ]
 where
  
  drawYaml value = 
    let bytesValue = Yaml.encode (value :: Yaml.Value)
        textValue = Text.Encode.decodeUtf8 bytesValue
        doHighLight = Bytes.length bytesValue < 2048
    in if doHighLight 
        then let preparedSource = prepareSource textValue 
             in B.vLimit (F.length preparedSource) $ B.renderRawSource B.txt preparedSource
        else B.vBox . map B.txt . Text.lines $ textValue

  source =
    either error id
      . S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) (S.defaultSyntaxMap Map.! "YAML")

  prepareSource txt = concatMap
    do
      reverse . map reverse . snd . F.foldl'
        do
          \(l, a) tok ->
            let tokText = snd tok
                maxWidth = availableSpace.width - 1
             in if Text.length tokText + l > maxWidth
                  then
                    let (x, y) = Text.splitAt (maxWidth - l) (snd tok)
                     in (Text.length y, [(fst tok, y)] : (a & _head %~ ((fst tok, x) :)))
                  else (l + Text.length tokText, a & _head %~ (tok :))
        do (0 :: Int, [[]])
    do source txt
