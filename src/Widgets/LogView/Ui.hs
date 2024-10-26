{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Widgets.LogView.Ui where

import Brick (clickable)
import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Lens (view)

import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (..))
import Data.Generics.Labels ()
import Data.Sequence qualified as Seq
import Type.AvailableSpace
import Type.Name
import Type.TBool (TBool, pattern Is)
import Widgets.Checkbox (drawCheckBox)
import Widgets.LogView.Tokenize (Token (..), TokenKind (..))
import Widgets.LogView.Types (
  CopyMethod (Native, Osc52),
  LogViewWidget (..),
  LogViewWidgetSettings (..),
  generateCache,
  mkName,
 )

logViewWidgetDraw :: TBool "active" -> AvailableSpace -> LogViewWidget -> B.Widget Name
logViewWidgetDraw isActive availableSpace LogViewWidget{..} =
  let LogViewWidgetSettings{..} = settings
   in B.reportExtent (mkName LogViewWidgetItself)
        . B.clickable (mkName LogViewWidgetItself)
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
          , B.padBottom B.Max
              . clickable (mkName LogViewWidgetContent)
              . B.cached (mkName LogViewWidgetContent)
              $ B.Widget
                { B.hSize = B.Greedy
                , B.vSize = B.Greedy
                , B.render = do
                    w' <- view B.availWidthL
                    h' <- view B.availHeightL
                    let w = w' - 1
                        withLength a = (a, either length length a)
                        (content, contLen) = case cache of
                          Just c -> (Right c, Seq.length c)
                          Nothing ->
                            withLength
                              . second Seq.fromList
                              . generateCache (if textWrap then Just w else Nothing) selectedLog jsonpathFilter
                              $ showJsonpath
                        d = fromIntegral @_ @Double
                        shownPercent = d h' / d contLen
                        offsetPercent = d offset / d contLen
                        scrollSize = max 1 $ ceiling @_ @Int $ shownPercent * d h'
                        offsetFize = max 0 $ min (h' - 1) $ ceiling @_ @Int $ offsetPercent * (d h' - d scrollSize)
                    B.render $
                      B.hBox
                        [ B.reportExtent (mkName LogViewWidgetContent) $
                            B.hLimit w $
                              either
                                do B.vBox . map B.txt
                                do draw . Seq.take h' . Seq.drop offset 
                                do content
                        , B.padLeft B.Max . B.vBox $
                            map
                              ( \i ->
                                  if i <= offsetFize
                                    then B.txt " "
                                    else
                                      if i <= (scrollSize + offsetFize)
                                        then B.clickable (mkName LogViewWidgetScrollBar) $ B.txt "|"
                                        else B.txt " "
                              )
                              [1 .. h']
                        ]
                }
          , B.hBox
              [ B.clickable (mkName LogViewWidgetCopyLog) $ B.txt "[Copy]"
              , B.padLeft B.Max $
                  B.hBox
                    [ B.clickable (mkName LogViewWidgetCopyMethod)
                        . B.txt
                        $ case copyMethod of
                          Osc52 -> "[Osc52]"
                          Native -> "[Native]"
                    , B.clickable (mkName LogViewWidgetWordWrap)
                        . B.txt
                        $ "[Wrap: " <> if textWrap then "yes]" else " no]"
                    ]
              ]
          ]
 where
  draw =
    let clickToCopy Token{..} = case kind of
          Key -> clickable (mkName $ LogViewWidgetCopyKey jsonpath)
          Bullet -> clickable (mkName $ LogViewWidgetCopyKey jsonpath)
          _ -> id
        colorize Token{..} =
          B.withAttr
            ( B.attrName "yaml" <> B.attrName case kind of
                Normal -> "normal"
                Key -> "key"
                String -> "string"
                Keyword -> "keyword"
                Bullet -> "bullet"
                Number -> "number"
                Plus -> "plus"
            )
     in B.vBox
          . map
            (B.hBox . map (\t@Token{..} -> clickToCopy t . colorize t $ B.txt text))
          . toList