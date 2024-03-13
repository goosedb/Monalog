{-# LANGUAGE DataKinds #-}

module Widgets.Query.Ui where

import Brick qualified as B
import Brick.Widgets.Edit qualified as B
import Data.Text qualified as Text
import Type.Name
import Type.TBool
import Widgets.Query.Types

queryWidgetDraw :: TBool "active" -> QueryWidget -> B.Widget Name
queryWidgetDraw isActive QueryWidget{..} =
  B.reportExtent queryWidgetName $
    B.hBox
      [ B.txt "Query: "
      , B.renderEditor (B.txt . Text.concat) (isActive == Is) input
      , case parseError of
          Just _ -> B.clickable (mkName QueryWidgetErrorHint) $ B.txt "[!]"
          Nothing -> B.emptyWidget
      ]
