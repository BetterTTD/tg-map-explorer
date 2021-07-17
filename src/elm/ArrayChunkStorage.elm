module ArrayChunkStorage exposing (ChunkStorage, getVisibleChunks, createDummyStorage)

import Array exposing (Array)
import Chunk exposing (Chunk)

type alias Cell =
  { horizontalOffset: Int
  , verticalOffset: Int
  , chunk: Chunk
  }

type alias ChunkStorage = Array Cell

inRange: Int -> (Int, Int) -> Bool
inRange value (left, right) =
  if value >= left && value <= right then True
  else False

isCellVisible: (Int, Int) -> (Int, Int) -> Cell -> Bool
isCellVisible hLim vLim cell =
  let
    hValue = cell.horizontalOffset
    vValue = cell.verticalOffset
  in
    (inRange hValue hLim) && (inRange vValue vLim)

createDummyCell: Int -> Int -> Int -> Cell
createDummyCell width _ ind =
  { horizontalOffset = (modBy width ind) * 8
  , verticalOffset = (ind // width) * 4
  , chunk = Array.repeat 32 0
  }

-- Делаем вид что размеры кратны размеру чанков
createDummyStorage: (Int, Int) -> ChunkStorage
createDummyStorage (width, height) =
  let
    columnsCount = width // 8
    rowsCount = height // 4
  in
    Array.initialize (columnsCount * rowsCount) (createDummyCell columnsCount rowsCount)


getVisibleChunks: (Int, Int) -> (Int, Int) -> ChunkStorage -> Array (Int, Int, Chunk)
getVisibleChunks hLim vLim storage =
  Array.filter (isCellVisible hLim vLim) storage
    |> Array.map (\cell -> (cell.horizontalOffset, cell.verticalOffset, cell.chunk))