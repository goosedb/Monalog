module SourceFormat.Utils where

import Data.Aeson qualified as J
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Time (getCurrentTime)
import Effectful
import Effectful qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import Type.Log

packLog :: (Eff.IOE :> es, Eff.State Int :> es) => C.ConduitT J.Value Log (Eff es) ()
packLog =
  C.mapM
    ( \json -> do
        idx <- Eff.get
        now <- liftIO getCurrentTime
        Eff.modify @Int (+ 1)
        pure $ Log (Idx idx) now json
    )
