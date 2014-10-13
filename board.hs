-- TODO: Actually make this a module

type Neighbours = Int
data CellContents = Full | Empty deriving (Eq)

instance Show CellContents where
  show Full = "█"
  show Empty = "·"

type Cell = (CellContents, Neighbours)

data Board = Board [[Cell]]

instance Show Board where
  show (Board []) = "Null board"
  show (Board [cells]) = concat $ map show $ map fst cells
  show (Board (cells:lines)) = show (Board [cells]) ++ "\n" ++ show (Board lines)

resetNeighbours :: Board -> Board
resetNeighbours (Board rows) = Board $ map resetRow rows
  where resetRow [] = []
        resetRow cells = map (\(contents, neighbours) -> (contents, 0)) cells

--calculateNeighbours :: Board -> Board

