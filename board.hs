-- TODO: Actually make this a module

data Cell = Full | Empty deriving (Eq)

instance Show Cell where
    show Full = "█"
    show Empty = "·"

data Board = Board [[Cell]]

instance Show Board where
    show (Board []) = "Null board"
    show (Board [cells]) = concat $ map show cells
    show (Board (line:lines)) = show (Board [line]) ++ "\n" ++ show (Board lines)
