{-# LANGUAGE DataKinds #-}

module Widgets.Types where

import Control.Lens (Lens')

type WithCallbacks cb s = (?callbacks :: cb s)

newtype PackedLens' s a = PackedLens' (Lens' s a)

type WithWidgetState s w = (?widgetState :: PackedLens' s w)

type WithWidgetContext s cb w = (WithCallbacks cb s, WithWidgetState s w)

widgetState :: (WithWidgetState s w) => Lens' s w
widgetState = let PackedLens' l = ?widgetState in l

withWidgetState :: Lens' s w -> ((WithWidgetState s w) => a) -> a
withWidgetState l a = let ?widgetState = PackedLens' l in a
