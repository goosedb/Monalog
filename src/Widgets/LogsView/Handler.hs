module Widgets.LogsView.Handler where

import Brick qualified as B
import Brick.BChan qualified as B
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Lens
import Control.Monad (forM_, forever, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Bool (bool)
import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MVec
import Query (Query)
import Query.Eval (QueryResult (..), evalQuery)
import Type.Event qualified as E
import Type.Log as Logs
import Type.Name
import Widgets.LogsView.Types

logsViewWidgetHandleEvent :: Lens' s LogsViewWidget -> LogsViewWidgetCallbacks s -> LogsWidgetEvent -> B.EventM Name s ()
logsViewWidgetHandleEvent widgetState LogsViewWidgetCallbacks{..} e = do
  currentTopLine <- use $ widgetState . #topLine
  currentTotalLines <- getTotalLines
  case e of
    Scroll int -> do
      handleScroll int
      resetFollowLogs
    AltScroll int -> do
      let vpName = mkName LogsViewWidgetViewport
      mbViewport <- B.lookupViewport vpName
      case mbViewport of
        Nothing -> handleScroll (int * 10)
        Just B.VP{..} | _vpSize == _vpContentSize -> handleScroll (int * 10)
        _ -> do
          B.invalidateCache
          B.hScrollBy (B.viewportScroll vpName) (int * 3)
      resetFollowLogs
    Click (LogsViewWidgetFieldMove dir) -> handleMoveClick dir
    Click (LogsViewWidgetLogEntry idx) -> handleLogEntryClick idx
    NewLog l -> do
      B.zoom widgetState do
        B.zoom #allLogs (addLog l)
        use #filterQueue >>= maybe (pure ()) (liftIO . atomically . flip writeTQueue l)
      use (widgetState . #activeLogs) >>= \case
        All -> do
          gotoBottomIfFollow
          B.zoom widgetState $ updateVisible #allLogs
          logAddedToView
        _ -> pure ()
    FilteredLog l -> do
      B.zoom (widgetState . #filteredLogs) (addLog l)
      use (widgetState . #activeLogs) >>= \case
        Filtered -> do
          gotoBottomIfFollow
          B.zoom widgetState $ updateVisible #filteredLogs
          logAddedToView
        _ -> pure ()
    RunFilter ch q -> B.zoom widgetState do
      runFilter ch q
    ClearFilter -> B.zoom widgetState clearFilter
    GoToTop -> do
      topLine <- use $ widgetState . #topLine
      handleScroll (-topLine)
    GoToBottom -> goToBottom
    GoTo n -> do
      topLine <- use $ widgetState . #topLine
      handleScroll (n - topLine)
    FollowLogs follow -> do
      widgetState . #followLogs .= follow
      gotoBottomIfFollow
    _ -> pure ()
  newTopLine <- use $ widgetState . #topLine
  when (currentTopLine /= newTopLine) do
    topLineChanged newTopLine
  newTotalLines <- getTotalLines
  when (currentTotalLines /= newTotalLines) do
    totalLinesChanged (snd newTotalLines)
 where
  resetFollowLogs = do
    resetFollow
    widgetState . #followLogs .= False

  goToBottom = do
    B.Extent{..} <- fromJust <$> B.lookupExtent (mkName LogsViewWidgetLogs)
    lastLine <- B.zoom widgetState do
      (.len) <$> getLogsToShow
    let newTopLine = max 1 $ lastLine - snd extentSize
    topLine <- use $ widgetState . #topLine
    when (newTopLine > topLine) do
      handleScroll (newTopLine - topLine + 1)

  gotoBottomIfFollow = do
    use (widgetState . #followLogs) >>= bool (pure ()) goToBottom

  getTotalLines =
    use (widgetState . #activeLogs) >>= \al ->
      (al,) <$> case al of
        All -> use $ widgetState . #allLogs . #len
        Filtered -> use $ widgetState . #filteredLogs . #len

  handleMoveClick dir = B.zoom widgetState do
    let fieldIndex = either id id dir
    let neighborIndex = either (subtract 1) (+ 1) dir
    field <- use $ #selectedFields . to (!! fieldIndex)
    neighbor <- use $ #selectedFields . to (!! neighborIndex)
    #selectedFields . ix fieldIndex .= neighbor
    #selectedFields . ix neighborIndex .= field
    B.invalidateCache

  handleLogEntryClick idx = do
    use (widgetState . #clickedLog) >>= \case
      Just selectedIdx
        | selectedIdx == idx -> do
            logEntry <- use (widgetState . #allLogs . #logs) >>= \l -> liftIO $ MVec.read l (idx - 1)
            selectedLog logEntry
        | otherwise -> B.invalidateCacheEntry . mkName $ LogsViewWidgetLogEntry selectedIdx
      Nothing -> pure ()
    widgetState . #clickedLog .= Just idx
    B.invalidateCacheEntry (mkName $ LogsViewWidgetLogEntry idx)

  getLogsToShow =
    use #activeLogs >>= \case
      All -> use #allLogs
      Filtered -> use #filteredLogs

  handleScroll (int :: Int) = do
    newTopLine <- B.zoom widgetState do
      logsToShow <- getLogsToShow
      logsViewHeight <- getLogsViewHeight
      newTopLine <- calculateNewTop logsToShow
      setImmutableLogs (newTopLine - 1) logsViewHeight logsToShow
      #topLine .= newTopLine
      B.invalidateCacheEntry (mkName LogsViewWidgetFiller)
      pure newTopLine
    topLineChanged newTopLine
   where
    calculateNewTop logsToShow = do
      topLine <- use #topLine
      pure
        if int < 0
          then max 1 (topLine - abs int)
          else min logsToShow.len (topLine + abs int)

getLogsViewHeight :: B.EventM Name s Int
getLogsViewHeight = do
  B.Extent{B.extentSize = (_, logViewH)} <- fromJust <$> B.lookupExtent (mkName LogsViewWidgetItself)
  pure (logViewH - 2)

setImmutableLogs :: Int -> Int -> Logs Mutable -> B.EventM Name LogsViewWidget ()
setImmutableLogs s l logs = do
  let len = max 0 $ min l (logs.len - s)
  if len == 0
    then #visibleLogs .= ImmutableLogs mempty
    else do
      slice <- liftIO $ Vec.freeze $ MVec.slice s (max 0 $ min l (logs.len - s)) logs.logs
      #visibleLogs .= ImmutableLogs slice

updateVisible :: Lens' LogsViewWidget (Logs Mutable) -> B.EventM Name LogsViewWidget ()
updateVisible logsLens = do
  logViewH <- getLogsViewHeight
  topLine <- use #topLine
  logs <- use logsLens
  B.invalidateCache
  setImmutableLogs (topLine - 1) logViewH logs

addLog :: Log -> B.EventM Name (Logs Mutable) ()
addLog l = do
  grewVec <- do
    MutableLogs{..} <- use id
    let capacity = MVec.length logs
    vec <- liftIO $ if capacity > len then pure logs else MVec.grow logs (capacity * 2)
    id .= MutableLogs{logs = vec, ..}
    pure vec
  len <- use $ to (.len)
  liftIO $ MVec.write grewVec len l
  #len += 1

runFilter :: B.BChan E.Event -> Query -> B.EventM Name LogsViewWidget ()
runFilter ch query = do
  filterQueue <- liftIO newTQueueIO
  vec <- liftIO $ MVec.new 64
  B.invalidateCache
  #filterQueue .= Just filterQueue
  #activeLogs .= Filtered
  #visibleLogs .= Logs.ImmutableLogs mempty
  #topLine .= 1
  #filteredLogs .= Logs.MutableLogs vec 0
  allLogs <- use #allLogs

  let filterAndSend l = when (filterLog query l) do
        B.writeBChan ch (E.FilteredLog l)

  workerId <- liftIO $ forkIO do
    forM_ [0 .. allLogs.len - 1] \i -> do
      l <- MVec.read allLogs.logs i
      filterAndSend l
    forever do
      l <- atomically (readTQueue filterQueue)
      filterAndSend l

  #filterWorker .= Just workerId

clearFilter :: B.EventM Name LogsViewWidget ()
clearFilter = do
  #filterQueue .= Nothing
  B.invalidateCache
  use #filterWorker >>= maybe (pure ()) (liftIO . killThread)
  #filterWorker .= Nothing
  #activeLogs .= All
  #topLine .= 1
  #visibleLogs .= Logs.ImmutableLogs mempty
  updateVisible #allLogs

filterLog :: Query -> Log -> Bool
filterLog query filteredLog = case evalQuery filteredLog.value query of
  BoolResult b -> b
  ValueResult (Bool b) -> b
  _ -> False
