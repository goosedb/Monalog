module Widgets.LogView.Ui where

import Brick qualified as B
import Brick.Widgets.Skylighting qualified as B
import Control.Lens
import Data.Aeson (Value)
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
import Widgets.LogView.Types
import Widgets.Scrollbar.Vertical qualified as VScroll

logViewWidgetDraw :: AvailableSpace -> LogViewWidget -> B.Widget Name
logViewWidgetDraw availableSpace LogViewWidget{..} =
  do
    B.hLimit availableSpace.width
    . B.withVScrollBarRenderer
      VScroll.renderer
    . B.withVScrollBars B.OnRight
    . B.viewport (mkName LogViewWidgetViewport) B.Vertical
    . B.cached (mkName LogViewWidgetItself)
    . B.vLimit (length preparedSource)
    . B.renderRawSource B.txt
    $ preparedSource
 where
  source =
    either error id
      . S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) (S.defaultSyntaxMap Map.! "YAML")
      . Text.Encode.decodeUtf8
      $ Yaml.encode @Value selectedLog.value

  preparedSource = concatMap
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
    do source
