module Widgets.Scrollbar.Vertical where

import Brick qualified as B
import Data.Maybe (fromJust)

renderer :: B.VScrollbarRenderer n
renderer =
  B.VScrollbarRenderer
    { renderVScrollbar = B.fill '║'
    , renderVScrollbarTrough = B.fill '│'
    , renderVScrollbarHandleBefore = B.emptyWidget
    , renderVScrollbarHandleAfter = B.emptyWidget
    , scrollbarWidthAllocation = 1
    }

handleScroll :: (Ord n) => B.Location -> B.Location -> n -> B.EventM n s ()
handleScroll (B.Location prevLoc) (B.Location newLoc) name = do
  B.VP{..} <- fromJust <$> B.lookupViewport name
  let ratio = fromIntegral @_ @Double (snd _vpContentSize) / fromIntegral (snd _vpSize)
  let diff = round $ fromIntegral (snd newLoc - snd prevLoc) * ratio
  B.vScrollBy (B.viewportScroll name) diff
