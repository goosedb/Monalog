module Widgets.Scrollbar.Vertical where

import Brick qualified as B

renderer :: B.VScrollbarRenderer n
renderer =
  B.VScrollbarRenderer
    { renderVScrollbar = B.fill '|'
    , renderVScrollbarTrough = B.fill ' '
    , renderVScrollbarHandleBefore = B.str ""
    , renderVScrollbarHandleAfter = B.str ""
    , scrollbarWidthAllocation = 1
    }
