module Widgets.Error.Handler where

import Brick qualified as B
import Type.Name
import Widgets.Error.Types

errorWidgetHandleEvent :: ErrorWidgetCallbacks s -> ErrorWidgetEvent -> B.EventM Name s ()
errorWidgetHandleEvent ErrorWidgetCallbacks{..} = \case
  Click ErrorWidgetOk -> closeErrorWidget
  _ -> pure ()
