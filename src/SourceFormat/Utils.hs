module SourceFormat.Utils where

import Data.Aeson qualified as J
import Data.Conduit qualified as C
import Data.Conduit.Combinators qualified as C
import Data.Time (UTCTime, getCurrentTime)
import Effectful
import Effectful qualified as Eff
import Effectful.State.Static.Local qualified as Eff
import Type.Log

newtype LogMeta = Timestamp UTCTime

packLog :: (Eff.IOE :> es, Eff.State Int :> es) => C.ConduitT (Maybe LogMeta, J.Value) Log (Eff es) ()
packLog =
  C.mapM
    ( \(meta, json) -> do
        idx <- Eff.get
        tm <- case meta of
          Just (Timestamp tm) -> pure tm
          _ -> liftIO getCurrentTime
        Eff.modify @Int (+ 1)
        pure $ Log (Idx idx) tm json
    )
