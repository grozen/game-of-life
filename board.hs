module Board
( Cell(..),
  Board,
  fromRowList,
  tick
) where

import qualified Data.Map as Map
import Control.Applicative
import Data.List
import Data.List.Split

data Cell = Full | Empty deriving (Eq)

instance Show Cell where
  show Full = "█"
  show Empty = "·"

data Board = Board {
           width :: Int,
           height :: Int,
           contents :: Map.Map (Int, Int) Cell
           }

boardCoordinates :: Board -> [(Int, Int)]
boardCoordinates board = (\x y -> (y, x)) <$> [1..(height board)] <*> [1..(width board)]

instance Show Board where
  show board =
    let coordinates = boardCoordinates board
        sequence = foldl (\string coordinate -> string ++ (show $ (contents board) Map.! coordinate)) "" coordinates
    in  unlines $ chunksOf (width board) sequence

fromRowList :: [[Cell]] -> Board
fromRowList [] = Board { width = 0, height = 0, contents = Map.empty }
fromRowList rows =
  let width' = foldl max 0 $ map length rows
      height' = length rows

      fullRows = map (\row -> take width' $ row ++ repeat Empty) rows

      processRow (y, contents') row = (y + 1, snd $ accumulateCells 1 y contents' row)

      accumulateCells x y contents' row = foldl (\(x, contents'') cell -> (x + 1, Map.insert (x, y) cell contents'')) (x, contents') row

      returnBoard (_, contents') = Board { width = width', height = height', contents = contents' }

  in  returnBoard $ foldl processRow (1, Map.empty) fullRows

tick :: Board -> Board
tick board =
  let coordinates = boardCoordinates board
      surroundingCoordinates (x, y) = map (\(x', y') -> (x + x', y + y')) (delete (0,0) $ (\x y -> (y, x)) <$> [-1..1] <*> [-1..1])

      neighbouringCells coordinate = map (\coordinate' -> Map.lookup coordinate' (contents board)) (surroundingCoordinates coordinate)

      neighbourCount coordinate = length (filter (\x -> x == Just Full) (neighbouringCells coordinate))

      calculateCell board' coordinate =
        Board { width = width board', height = height board',
                contents = Map.insert coordinate (cellValue ((contents board) Map.! coordinate) $ neighbourCount coordinate) (contents board') }

      cellValue Full count
        | count < 2 = Empty
        | count < 4 = Full
        | otherwise = Empty
      cellValue Empty count
        | count == 3 = Full
        | otherwise  = Empty

      in foldl calculateCell board coordinates
