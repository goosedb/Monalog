module Widgets.Dialog.Handler where

import Brick qualified as B
import Control.Lens (Lens', use, (%=))
import Data.Foldable (find)
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe)
import Type.Name
import Widgets.Dialog.Types

dialogWidgetHandleEvent :: Lens' s (Maybe (DialogWidget s)) -> DialogWidgetEvent -> B.EventM Name s ()
dialogWidgetHandleEvent widgetState = \case
  Click (DialogButton n) ->
    use widgetState >>= \mw -> fromMaybe (pure ()) do
      DialogWidget{..} <- mw
      (_, action) <- find ((== n) . fst) actions
      pure action
  SelectLeft ->
    widgetState %= maybe Nothing \DialogWidget{..} ->
      Just $
        DialogWidget
          { selected = max 0 (selected - 1)
          , ..
          }
  SelectRight ->
    widgetState %= maybe Nothing \DialogWidget{..} ->
      Just $
        DialogWidget
          { selected = min (selected + 1) (length actions - 1)
          , ..
          }
  Accept -> do
    use widgetState >>= \case
      Just DialogWidget{..} -> snd $ actions !! selected
      Nothing -> pure ()
  _ -> pure ()
