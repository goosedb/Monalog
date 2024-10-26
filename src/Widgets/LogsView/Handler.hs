module Widgets.LogsView.Handler where

import Brick qualified as B
import Brick.BChan qualified as B
import Buffer (Buffer (..), makeBuffer)
import Consts (initialTopLine)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Lens
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Generics.Labels ()
import Data.Maybe (fromJust)
import Data.Vector qualified as Vec
import Data.Vector.Mutable qualified as MVec
import Graphics.Vty qualified as V
import Query (Query)
import Query.Eval (QueryResult (..), evalQuery)
import Type.Event qualified as E
import Type.Log as Logs
import Type.Name
import Widgets.LogsView.Types
import Widgets.Scrollbar.Horizontal qualified as HScroll
import Widgets.Types

type Ctx s = (WithWidgetContext s LogsViewWidgetCallbacks LogsViewWidget)

logsViewWidgetHandleEvent :: (Ctx s) => LogsWidgetEvent -> B.EventM Name s ()
logsViewWidgetHandleEvent e = do
  case e of
    Scroll (signum -> int) -> move int
    AltScroll (signum -> int) -> scrollHorizontally int
    Key key mods -> handleKeyboardEvent key mods
    Click (LogsViewWidgetFieldMove dir) -> handleMoveClick dir
    Click (LogsViewWidgetLogEntry lineNumber _) -> handleMouseSelect lineNumber
    Move (LogsViewWidgetHScrollBar _) prevLoc newLoc -> do
      HScroll.handleScroll prevLoc newLoc (mkName LogsViewWidgetViewport)
      B.invalidateCache
    NewLog l -> do
      B.zoom widgetState do
        B.zoom #allLogs (addLog l)
        newLogsLen <- use $ #allLogs . to (.len)
        when (isPowerOfTen newLogsLen) B.invalidateCache
        use #filterQueue >>= maybe (pure ()) (liftIO . atomically . flip writeTQueue l)

      use (widgetState . #activeLogs) >>= \case
        All -> do
          addLogsToView
          updateTotalLines
        _ -> pure ()
    FilteredLog l -> do
      B.zoom (widgetState . #filteredLogs) (addLog l)

      use (widgetState . #activeLogs) >>= \case
        Filtered -> do
          addLogsToView
          updateTotalLines
        _ -> pure ()
    RunFilter ch q -> runFilter ch q
    ClearFilter -> clearFilter
    GoToTop -> goToTop
    GoToBottom -> goToBottom
    GoTo n -> do
      goTo (Line n)
      use (widgetState . #topLine) >>= selectByLine . MkLineNumber
    FollowLogs follow -> do
      widgetState . #followLogs .= follow
      goToBottom
    CleanupLogs -> do
      defaultSelectedFields <- use $ widgetState . #defaultSelectedFields
      B.zoom widgetState do
        allLogs <- liftIO $ MVec.new 64
        filteredLogs <- liftIO $ MVec.new 1
        #selectedFields .= defaultSelectedFields
        #allLogs .= MutableLogs allLogs 0
        #filteredLogs .= MutableLogs filteredLogs 0
        #visibleLogs .= ImmutableLogs mempty

      ?callbacks.topLineChanged initialTopLine
      ?callbacks.totalLinesChanged 0
      B.invalidateCache
    _ -> pure ()
 where
  addLogsToView = do
    thereIsSpaceForLogInView <- B.zoom widgetState do
      getThereIsSpaceForLogInView

    use (widgetState . #followLogs) >>= bool
      do
        when thereIsSpaceForLogInView do
          B.zoom widgetState updateVisibleLogs
      do
        goTo Bottom
        B.zoom widgetState updateVisibleLogs

    ?callbacks.logAddedToView

  getThereIsSpaceForLogInView = do
    visibleLogs <- use #visibleLogs
    visibleAreaHeight <- getLogsViewHeight
    pure (visibleAreaHeight > Vec.length visibleLogs.logs)

  handleMoveClick dir = B.zoom widgetState do
    let fieldIndex = either id id dir
    let neighborIndex = either (subtract 1) (+ 1) dir
    field <- use $ #selectedFields . to (!! fieldIndex)
    neighbor <- use $ #selectedFields . to (!! neighborIndex)
    #selectedFields . ix fieldIndex .= neighbor
    #selectedFields . ix neighborIndex .= field
    B.invalidateCache

scrollHorizontally :: Int -> B.EventM Name s ()
scrollHorizontally i =
  let vpName = mkName LogsViewWidgetViewport
   in B.hScrollBy (B.viewportScroll vpName) i

addLog :: Log -> B.EventM Name (Logs Mutable) ()
addLog l = do
  grewVec <- do
    MutableLogs{..} <- use id
    let capacity = MVec.length logs
    vec <- liftIO $ if capacity > len then pure logs else MVec.grow logs (capacity * 2)
    id .= MutableLogs{logs = vec, ..}
    pure vec
  len <- use #len
  liftIO $ MVec.write grewVec len l
  #len += 1

runFilter :: (Ctx s) => B.BChan E.Event -> Query -> B.EventM Name s ()
runFilter ch query = do
  filterQueue <- liftIO newTQueueIO
  vec <- liftIO $ MVec.new 64
  changeTopLine minimalTopLine
  B.zoom widgetState do
    #activeLogs .= Filtered
    #filteredLogs .= Logs.MutableLogs vec 0
    #filterQueue .= Just filterQueue
    updateVisibleLogs

    allLogs <- use #allLogs

    Buffer{..} <- liftIO $ makeBuffer (B.writeBChan ch . E.FilteredLogs)

    let filterAndSend l = when (filterLog l) do
          pushBuffer l

    workerId <- liftIO $ forkIO do
      forM_ [0 .. allLogs.len - 1] \i -> do
        MVec.read allLogs.logs i >>= filterAndSend
      forever do
        atomically (readTQueue filterQueue) >>= filterAndSend

    #killFilterWorker .= (killThread workerId >> killBuffer)

  B.invalidateCache
 where
  filterLog filteredLog = case evalQuery filteredLog.value query of
    BoolResult b -> b
    ValueResult (Bool b) -> b
    _ -> False

clearFilter :: (Ctx s) => B.EventM Name s ()
clearFilter = do
  changeTopLine minimalTopLine
  B.zoom widgetState do
    #filterQueue .= Nothing
    use #killFilterWorker >>= liftIO
    #killFilterWorker .= pure ()
    #activeLogs .= All
    updateVisibleLogs

  updateTotalLines

  B.invalidateCache

updateTotalLines :: (Ctx s) => B.EventM Name s ()
updateTotalLines = B.zoom widgetState getActiveLogs >>= ?callbacks.totalLinesChanged . (.len)

data GoTo = Line Int | Bottom | Top | Relative Int

handleKeyboardEvent :: (Ctx s) => V.Key -> [V.Modifier] -> B.EventM Name s ()
handleKeyboardEvent key mods = do
  case key of
    _ | key == V.KChar 'H' || shifted V.KLeft -> do
      scrollHorizontally (-longHScroll)
    _
      | key == V.KChar 'L' || shifted V.KRight ->
          scrollHorizontally longHScroll
    _ | key == V.KChar 'K' || shifted V.KDown -> move longVScroll
    _ | key == V.KChar 'J' || shifted V.KUp -> move (-longVScroll)
    V.KChar 't' -> goToTop
    V.KChar 'b' -> goToBottom
    _ | key `elem` [V.KChar 'h', V.KLeft] -> scrollHorizontally (-shortScroll)
    _ | key `elem` [V.KChar 'l', V.KRight] -> scrollHorizontally shortScroll
    _ | key `elem` [V.KChar 'j', V.KUp] -> move (-shortScroll)
    _ | key `elem` [V.KChar 'k', V.KDown] -> move shortScroll
    V.KChar 's' -> use (widgetState . #topLine) >>= handleKeyboardSelect . MkLineNumber
    V.KChar 'g' -> ?callbacks.goToRequest
    V.KChar _ -> pure ()
    V.KEnter ->
      use (widgetState . #selectedLog) >>= maybe
        do pure ()
        do ?callbacks.selectedLog . Just . snd
    _ -> pure ()
 where
  shifted b = key == b && mods == [V.MShift]

goToTop :: (Ctx s) => B.EventM Name s ()
goToTop =
  goTo Top
    >> B.invalidateCache
    >> deselectLog

goToBottom :: (Ctx s) => B.EventM Name s ()
goToBottom =
  goTo Bottom
    >> B.invalidateCache
    >> deselectLog

handleKeyboardSelect :: (Ctx s) => LineNumber -> B.EventM Name s ()
handleKeyboardSelect line =
  use (widgetState . #selectedLog) >>= maybe
    do selectByLine line
    do const deselectLog

handleMouseSelect :: (Ctx s) => LineNumber -> B.EventM Name s ()
handleMouseSelect line = do
  alreadySelected <- use (widgetState . #selectedLog)
  selecting <- B.zoom widgetState (getLineLog line)
  if ((.idx) . snd <$> alreadySelected) == ((.idx) <$> selecting)
    then deselectLog
    else traverse_ (select line) selecting

deselectLog :: (Ctx s) => B.EventM Name s ()
deselectLog = do
  alreadySelected <- use (widgetState . #selectedLog)
  case alreadySelected of
    Just (ln, Log{idx}) -> B.invalidateCacheEntry (mkName $ LogsViewWidgetLogEntry ln idx)
    Nothing -> pure ()
  widgetState . #selectedLog .= Nothing
  ?callbacks.selectedLog Nothing

move :: (Ctx s) => Int -> B.EventM Name s ()
move diff =
  use (widgetState . #selectedLog) >>= maybe
    do goTo (Relative diff)
    do
      \(MkLineNumber lineNumber, _) -> do
        topLine <- use $ widgetState . #topLine
        logsViewHeight <- getLogsViewHeight
        logs <- B.zoom widgetState getActiveLogs
        let middle = topLine + (logsViewHeight `div` 2)
        let newLineNumber = min logs.len $ max minimalTopLine $ lineNumber + diff
        selectByLine (MkLineNumber newLineNumber)
        if
          | newLineNumber <= topLine -> goTo (Relative (newLineNumber - topLine))
          | topLine + logsViewHeight <= logs.len + 1 -> when (newLineNumber > middle) do
              goTo (Relative (newLineNumber - middle))
          | otherwise -> pure ()

selectByLine :: (Ctx s) => LineNumber -> B.EventM Name s ()
selectByLine line = do
  B.zoom widgetState (getLineLog line) >>= maybe (pure ()) (select line)

getLogById :: (Ctx s) => Idx -> B.EventM Name s Log
getLogById idx = use (widgetState . #allLogs . #logs) >>= liftIO . ($ idx.rawIdx - 1) . MVec.read

select :: (Ctx s) => LineNumber -> Log -> B.EventM Name s ()
select lineNumber selectingLog = do
  deselectLog
  widgetState . #selectedLog ?= (lineNumber, selectingLog)
  B.invalidateCacheEntry (mkName $ LogsViewWidgetLogEntry lineNumber selectingLog.idx)
  ?callbacks.selectedLog (Just selectingLog)

goTo :: forall s. (Ctx s) => GoTo -> B.EventM Name s ()
goTo gt = do
  topLine <- use $ widgetState . #topLine
  case gt of
    Bottom -> pure ()
    Relative n | n > 0 -> pure ()
    _ -> resetFollowLogs

  let goToLine line = do
        logs <- B.zoom widgetState getActiveLogs
        let lineToSet = max minimalTopLine . min logs.len $ line
        when (lineToSet /= topLine) do
          changeTopLine lineToSet
          B.zoom widgetState updateVisibleLogs

  case gt of
    Top -> goToLine minimalTopLine
    Bottom -> do
      logsViewHeight <- getLogsViewHeight
      logs <- B.zoom widgetState getActiveLogs
      when (topLine + logsViewHeight <= logs.len) do
        changeTopLine $ (logs.len - logsViewHeight + 1) + if logs.len > 0 then 1 else 0
        B.zoom widgetState updateVisibleLogs
    Line line -> goToLine line
    Relative diff -> goToLine (topLine + diff)

updateVisibleLogs :: B.EventM Name LogsViewWidget ()
updateVisibleLogs = do
  topLine <- use #topLine
  viewHeight <- getLogsViewHeight
  logs <- getActiveLogs
  let sliceLength = max 0 $ min (logs.len - topLine + 1) viewHeight
  slice <-
    if sliceLength > 0
      then liftIO $ Vec.freeze $ MVec.slice (topLine - 1) sliceLength logs.logs
      else pure mempty
  #visibleLogs .= ImmutableLogs slice

getLogsViewHeight :: B.EventM Name s Int
getLogsViewHeight = do
  B.Extent{B.extentSize = (_, logViewH)} <- fromJust <$> B.lookupExtent (mkName LogsViewWidgetItself)
  pure (logViewH - 2)

getActiveLogs :: B.EventM Name LogsViewWidget (Logs Mutable)
getActiveLogs =
  use #activeLogs >>= \case
    All -> use #allLogs
    Filtered -> use #filteredLogs

getLineLog :: (Ctx s) => LineNumber -> B.EventM Name LogsViewWidget (Maybe Log)
getLineLog line = do
  logs <- getActiveLogs
  traverse (liftIO . MVec.read logs.logs) case line of
    _ | logs.len == 0 -> Nothing
    MkLineNumber (pred . max minimalTopLine . min logs.len -> n) -> Just n

longVScroll :: Int
longVScroll = 10

longHScroll :: Int
longHScroll = 3

shortScroll :: Int
shortScroll = 1

resetFollowLogs :: (Ctx s) => B.EventM Name s ()
resetFollowLogs = do
  ?callbacks.resetFollow
  widgetState . #followLogs .= False

ifFalse :: (Applicative f) => f a -> Bool -> f ()
ifFalse action = bool (void action) (pure ())

changeTopLine :: (Ctx s) => Int -> B.EventM Name s ()
changeTopLine i = do
  widgetState . #topLine .= i
  ?callbacks.topLineChanged i

powersOfTen :: [Int]
powersOfTen = iterate (* 10) 10

isPowerOfTen :: Int -> Bool
isPowerOfTen n = (== n) $ head $ dropWhile (< n) powersOfTen
