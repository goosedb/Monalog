module Widgets.Scrollbar.Horizontal where

import Brick qualified as B
import Data.Maybe (fromJust)

renderer :: B.HScrollbarRenderer n
renderer =
  B.HScrollbarRenderer
    { renderHScrollbar = B.fill '═'
    , renderHScrollbarTrough = B.fill '─'
    , renderHScrollbarHandleBefore = B.str " "
    , renderHScrollbarHandleAfter = B.str " "
    , scrollbarHeightAllocation = 1
    }

handleScroll :: (Ord n) => B.Location -> B.Location -> n -> B.EventM n s ()
handleScroll (B.Location prevLoc) (B.Location newLoc) name = do
  B.VP{..} <- fromJust <$> B.lookupViewport name
  let ratio = fromIntegral @_ @Double (fst _vpContentSize) / fromIntegral (fst _vpSize)
  let diff = round $ fromIntegral (fst newLoc - fst prevLoc) * ratio
  B.hScrollBy (B.viewportScroll name) diff
