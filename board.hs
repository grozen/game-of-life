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
  show (Board (cells:rows)) = show (Board [cells]) ++ "\n" ++ show (Board rows)

resetNeighbours :: Board -> Board
resetNeighbours (Board rows) = Board $ map resetRow rows
  where resetRow [] = []
        resetRow cells = map (\(contents, neighbours) -> (contents, 0)) cells

calculateNeighbours :: Board -> Board
calculateNeighbours (Board rows) = Board $ pushNeighbours $ pullNeighbours rows
  where pushNeighbours [] = []
        pushNeighbours [[]] = [[]]
        pushNeighbours [cells] = [pushRow cells]
        pushNeighbours [cells:nextCells:rows] = (pushRow cells):(pushNeighbours (pushNextRow cells nextCells):rows)
        pushRow [] = []
        pushRow [_] = [_]
        pushRow first@(Full, _):(content, neighbours):cells = first:(pushRow (content, neighbours + 1):cells)
        updateNextRow [] nextCells = nextCells
        -- Maybe some take 2 drop 2 action here?
        -- updateNextRow [(Full, _)] nextCells = nextCells

