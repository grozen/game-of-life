-- TODO: Actually make this a module
import qualified Data.Map as Map
import Control.Applicative
import Data.List.Split

data Cell = Full | Empty

instance Show Cell where
  show Full = "█"
  show Empty = "·"

data Board = Board {
           width :: Int,
           height :: Int,
           contents :: Map.Map (Int, Int) Cell
           }

instance Show Board where
  show board =
    let coordinates = (\x y -> (y, x)) <$> [1..(height board)] <*> [1..(width board)]
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
