module Widgets.LogsView.Ui where

import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as B
import Control.Lens
import Data.Aeson (Value (..), encode)
import Data.Aeson.Lens (key)
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Foldable qualified as F
import Data.Generics.Labels ()
import Data.List (intersperse)
import Data.Scientific qualified as S
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time.Format qualified as Time
import Graphics.Vty qualified as V
import Type.AvailableSpace
import Type.Field (Field (..), drawLogsViewColumnHeaderTxt)
import Type.Log as Logs
import Type.MaxWidth
import Type.Name
import Widgets.LogsView.Types
import Widgets.Scrollbar.Horizontal qualified as HScroll

logsViewWidgetDraw :: AvailableSpace -> LogsViewWidget -> B.Widget Name
logsViewWidgetDraw availableSpace LogsViewWidget{..} = B.clickable (mkName LogsViewWidgetItself) $
  withHorizontalScroll do
    B.vBox
      [ B.cached (mkName LogsViewWidgetNumberHeader)
          . B.vLimit 1
          . B.hBox
          $ numberColumnHeader : intersperse B.vBorder otherHeaders
      , B.hBorder
      , B.reportExtent (mkName LogsViewWidgetLogs) $
          B.vBox
            [ let logsList = F.toList visibleLogs.logs
               in B.vBox $ zip [topLine ..] logsList <&> logRow
            , filler
            ]
      ]
 where
  withHorizontalScroll =
    if needViewport
      then
        B.withClickableHScrollBars (\e _ -> mkName (LogsViewWidgetHScrollBar e))
          . B.withHScrollBarRenderer
            HScroll.renderer
          . B.withHScrollBars B.OnBottom
          . B.viewport (mkName LogsViewWidgetViewport) B.Horizontal
          . B.hLimit totalWidth
      else id

  widths = calculateWidths selectedFields availableSpace.width logNumberWidth

  needViewport = totalWidth > availableSpace.width

  totalWidth = sum (map (.width) widths) + fieldsNumber + logNumberWidth

  numberColumnHeader =
    B.hBox
      [ B.hLimit logNumberWidth (B.padRight B.Max $ B.str " #")
      , B.vBorder
      ]

  logNumberWidth = length (show allLogs.len) + 2

  fieldsNumber = length selectedFields

  otherHeaders = map
    do
      \FieldWidth{..} ->
        let titleWidth = if isFirstIndex idx || isLastIndex fieldsNumber idx then width - cornerColumnExtraSpace else width - columnExtraSpace
            leftArrowName = mkName $ LogsViewWidgetFieldMove (Left idx)
            rightArrowName = mkName $ LogsViewWidgetFieldMove (Right idx)
         in B.hBox
              [ if isFirstIndex idx then B.txt " " else B.clickable leftArrowName $ B.txt " <"
              , B.cached (mkName $ LogsViewWidgetFieldHeader field)
                  . B.hLimit titleWidth
                  . B.hCenter
                  $ drawLogsViewColumnHeader field
              , if isLastIndex fieldsNumber idx then B.txt " " else B.clickable rightArrowName $ B.txt "> "
              ]
    do widths

  filler =
    B.clickable
      (mkName LogsViewWidgetFiller)
      let
        numColumn = B.hLimit logNumberWidth (B.fill ' ')
        otherColumns = case widths of
          [] -> [B.fill ' ']
          ws -> map (\FieldWidth{width} -> B.hLimit width (B.fill ' ')) ws
        row = B.hBox $ intersperse B.vBorder (numColumn : otherColumns)
       in
        B.vBox $ map (const row) [1 .. availableSpace.height]

  logRow (i, l@Log{..}) =
    let drawColumn field = drawLogsViewLogField field l B.<+> B.fill ' '
        logNum = B.hLimit logNumberWidth $ B.str (" " <> show i) B.<+> B.fill ' '
        highlight = if ((.idx) . snd <$> selectedLog) == Just idx then B.modifyDefAttr (\V.Attr{..} -> V.Attr{attrStyle = V.SetTo V.reverseVideo, ..}) else id
        name = mkName $ LogsViewWidgetLogEntry (MkLineNumber i) idx
     in B.clickable name
          . B.cached name
          . highlight
          . B.vLimit 1
          . B.hBox
          $ logNum
            : B.vBorder
            : intersperse B.vBorder (map (\FieldWidth{width, field} -> B.hLimit width (drawColumn field)) widths)

  drawLogsViewColumnHeader = B.txt . drawLogsViewColumnHeaderTxt

  drawLogsViewLogField field Log{..} = case field of
    Timestamp -> B.str $ Time.formatTime Time.defaultTimeLocale "%H:%M:%S" timestamp
    Raw -> jsonWidget value
    Field path -> maybe
      do B.emptyWidget
      do jsonWidget
      do F.foldl' (\v k -> v >>= (^? key k)) (Just value) path

  jsonWidget = \case
    String t -> B.txt t
    Bool b -> B.str (show b)
    Number n
      | S.isInteger n -> B.str (show $ round @_ @Integer n)
      | otherwise -> B.str (show n)
    v -> B.txt . Text.Encoding.decodeUtf8 . Bytes.Lazy.toStrict . encode $ v

isFirstIndex :: Int -> Bool
isFirstIndex = (== 0)

isLastIndex :: Int -> Int -> Bool
isLastIndex len = (== len - (1 :: Int))

cornerColumnExtraSpace :: Int
cornerColumnExtraSpace = 3

columnExtraSpace :: Int
columnExtraSpace = 4

calculateWidths :: [SelectedField] -> Int -> Int -> [FieldWidth]
calculateWidths selectedFields availableWidth logNumberWidth =
  let logNumberColumnWidth = logNumberWidth

      columnsNumber = length selectedFields

      indexedFields = zip [0 ..] selectedFields

      columnSeparatorsNumber = columnsNumber

      minimalWidth = 15

      expectedSpaceForColumns = availableWidth - logNumberColumnWidth - columnSeparatorsNumber
      expectedSpacePerColumn = expectedSpaceForColumns `div` columnsNumber

      columnWidth (idx, field) =
        let headerWidth =
              Text.length (drawLogsViewColumnHeaderTxt field.field)
                + if isFirstIndex idx || isLastIndex columnsNumber idx then cornerColumnExtraSpace else columnExtraSpace
         in max headerWidth $ succ field.width.rawMaxWidth

      isCompactColumn (idx, col) = columnWidth (idx, col) <= expectedSpacePerColumn

      compactColumns = filter isCompactColumn indexedFields
      wideColumnsNumber = length $ filter (not . isCompactColumn) indexedFields

      spaceForWideColumns = expectedSpaceForColumns - sum (map columnWidth compactColumns)
      spaceForWideColumn = spaceForWideColumns `div` wideColumnsNumber
   in indexedFields <&> \indexedColumn@(idx, column) ->
        let widthOfColumn =
              if isCompactColumn indexedColumn
                then columnWidth indexedColumn
                else max minimalWidth spaceForWideColumn
         in FieldWidth idx widthOfColumn column.field
