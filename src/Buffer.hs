module Buffer where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically, flushTQueue, newTQueueIO, newTVarIO, readTVar, retry, writeTQueue, writeTVar)
import Control.Monad (forever)
import Data.Bool (bool)

data Buffer a = Buffer {killBuffer :: IO (), pushBuffer :: a -> IO ()}

makeBuffer :: ([a] -> IO b) -> IO (Buffer a)
makeBuffer sink = do
  canFlush <- newTVarIO True
  buffer <- newTQueueIO
  throttler <- forkIO $ forever do
    threadDelay 250000
    atomically do
      readTVar canFlush >>= bool (pure ()) retry
      writeTVar canFlush True
  flusher <- forkIO $ forever do
    vals <- atomically do
      readTVar canFlush >>= bool retry (pure ())
      flushTQueue buffer >>= \case
        [] -> retry
        vals -> pure vals
    atomically do
      writeTVar canFlush False
    sink vals
  pure
    Buffer
      { pushBuffer = atomically . writeTQueue buffer
      , killBuffer = killThread throttler >> killThread flusher
      }
