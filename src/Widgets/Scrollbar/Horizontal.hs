module Widgets.Scrollbar.Horizontal where

import Brick qualified as B

renderer :: B.HScrollbarRenderer n
renderer =
  B.HScrollbarRenderer
    { renderHScrollbar = B.fill '='
    , renderHScrollbarTrough = B.fill ' '
    , renderHScrollbarHandleBefore = B.str "["
    , renderHScrollbarHandleAfter = B.str "]"
    , scrollbarHeightAllocation = 1
    }
