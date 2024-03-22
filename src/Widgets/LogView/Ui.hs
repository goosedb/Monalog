{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Widgets.LogView.Ui where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Brick.Widgets.Skylighting qualified as B
import Control.Lens
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.JSONPath (JSONPathElement (..))
import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific)
import Data.Sequence qualified as Seq
import Data.Text (Text)
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
            , B.clickable (mkName LogViewWidgetCopyLog) $ B.txt "[Copy]"
            ]
        , B.withVScrollBarRenderer
            VScroll.renderer
            . B.withVScrollBars B.OnRight
            . B.viewport (mkName LogViewWidgetViewport) B.Vertical
            . B.cached (mkName LogViewWidgetItself)
            $ if showJsonpath
              then either
                do B.vBox . map B.txtWrap . Text.lines
                do
                  \val ->
                    let preparedSource = prepareSource val
                     in B.vLimit (F.length preparedSource)
                          . B.renderRawSource B.txt
                          $ preparedSource
                do jsonpathFilteredValue
              else
                let preparedSource = prepareSource value
                 in B.vLimit (F.length preparedSource)
                      . B.renderRawSource B.txt
                      $ preparedSource
        ]
 where
  source val =
    either error id
      . S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) (S.defaultSyntaxMap Map.! "YAML")
      . Text.Encode.decodeUtf8
      $ Yaml.encode val

  prepareSource val = concatMap
    do
      reverse . map reverse . snd . F.foldl'
        do
          \(l, a) tok ->
            let tokText = snd tok
             in if Text.length tokText + l > availableSpace.width
                  then
                    let (x, y) = Text.splitAt (availableSpace.width - l) (snd tok)
                     in (Text.length y, [(fst tok, y)] : (a & _head %~ ((fst tok, x) :)))
                  else (l + Text.length tokText, a & _head %~ (tok :))
        do (0 :: Int, [[]])
    do source val

data Token = Key Path Text | ListItem Path | Number Scientific | Boolean Bool | Text Text | Space Int | Null | Colon | NewLine

type Path = Seq.Seq JSONPathElement
data TokenizationState = TokenizationState
  { path :: Path
  , ident :: Int
  , limit :: Int
  , column :: Int
  }

split :: Int -> Text -> [Text]
split n (Text.splitAt n -> (a, b)) = a : if Text.null b then [] else split n b

showt :: (Show a) => a -> Text
showt = Text.pack . show
