module AttrMap where

import Brick.AttrMap qualified as B
import Data.Bifunctor (Bifunctor (..))
import Graphics.Vty (black, blue, defAttr, green, magenta, red, withForeColor, yellow)

yaml :: B.AttrMap
yaml =
  B.attrMap defAttr . map (first ((B.attrName "yaml" <>) . B.attrName)) $
    [ ("normal", defAttr)
    , ("string", defAttr `withForeColor` green)
    , ("keyword", defAttr `withForeColor` red)
    , ("bullet", defAttr `withForeColor` blue)
    , ("number", defAttr `withForeColor` magenta)
    , ("key", defAttr `withForeColor` yellow)
    , ("plus", defAttr `withForeColor` black)
    ]
