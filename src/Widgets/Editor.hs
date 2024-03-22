module Widgets.Editor where

import Brick.Types qualified as B
import Brick.Widgets.Edit qualified as B
import Control.Lens ((%=))
import Data.Text (Text)
import Data.Text.Zipper qualified as Z
import Data.Text.Zipper.Generic.Words qualified as Z
import Graphics.Vty qualified as V
import Type.Name

handleEditorEvent :: V.Key -> [V.Modifier] -> B.EventM Name (B.Editor Text Name) ()
handleEditorEvent k m = do
  case (k, m) of
    (V.KLeft, [V.MCtrl]) -> B.editContentsL %= Z.moveWordLeft
    (V.KRight, [V.MCtrl]) -> B.editContentsL %= Z.moveWordRight
    (V.KLeft, []) -> B.editContentsL %= Z.moveLeft
    (V.KRight, []) -> B.editContentsL %= Z.moveRight
    (V.KChar c, ms) -> B.handleEditorEvent (B.VtyEvent $ V.EvKey (V.KChar c) ms)
    (V.KBS, []) -> B.handleEditorEvent (B.VtyEvent $ V.EvKey V.KBS [])
    (V.KBS, [V.MCtrl]) -> do
      B.editContentsL %= Z.deletePrevWord
    (V.KDel, []) -> B.handleEditorEvent (B.VtyEvent $ V.EvKey V.KDel [])
    (V.KDel, [V.MCtrl]) -> B.editContentsL %= Z.deleteWord
    _ -> pure ()

emptyEditor :: n -> B.Editor Text n
emptyEditor name = B.editorText name (Just 1) ""
