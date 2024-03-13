module Widgets.LogView.Handler where

import Brick qualified as B
import Control.Lens
import Data.Generics.Labels ()
import Type.Name (LogViewWidgetName (..), Name)
import Widgets.LogView.Types

logViewWidgetHandleEvent :: Traversal' s LogViewWidget -> LogViewWidgetEvent -> B.EventM Name s ()
logViewWidgetHandleEvent widgetState = \case
  LogSelected l -> do
    B.invalidateCacheEntry (mkName LogViewWidgetItself)
    B.vScrollToBeginning (B.viewportScroll (mkName LogViewWidgetViewport))
    widgetState . #selectedLog .= l
  Scroll i -> do
    B.vScrollBy (B.viewportScroll (mkName LogViewWidgetViewport)) i
