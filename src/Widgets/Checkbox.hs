module Widgets.Checkbox where

import Brick qualified as B
import Data.Text
import Type.Name

drawCheckBox :: Bool -> Name -> Text -> B.Widget Name
drawCheckBox isActive name txt =
  B.clickable name $
    B.hBox
      [ B.txt if isActive then "[x]" else "[ ]"
      , B.txt " "
      , B.txt txt
      ]
