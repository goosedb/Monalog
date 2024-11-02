module AttrMap where

import Brick.AttrMap qualified as B
import Data.Bifunctor (Bifunctor (..))
import Graphics.Vty (blue, cyan, defAttr, green, magenta, red, reverseVideo, withForeColor, withStyle, yellow)

attrs :: B.AttrMap
attrs = B.attrMap defAttr (yaml <> hint)
 where
  yaml =
    map (first ((B.attrName "yaml" <>) . B.attrName)) $
      [ ("normal", defAttr)
      , ("string", defAttr `withForeColor` green)
      , ("keyword", defAttr `withForeColor` red)
      , ("bullet", defAttr `withForeColor` blue)
      , ("number", defAttr `withForeColor` magenta)
      , ("key", defAttr `withForeColor` yellow)
      , ("plus", defAttr `withForeColor` cyan)
      ]

  hint = [(B.attrName "hint", defAttr `withStyle` reverseVideo)]
