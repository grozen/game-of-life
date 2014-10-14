import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad.Loops
import qualified Board as Board

binaryToCell '1' = Board.Full
binaryToCell '0' = Board.Empty

tickBoard :: Board -> IO Board
tickBoard board =
  let ticked = tick board
  in  threadDelay 1000000 >> print ticked >> return ticked

main = do
  boardFile:_ <- getArgs
  handle <- openFile boardFile ReadMode
  contents <- hGetContents handle
  board <- return (Board.fromRowList $ map (\row -> map (\char -> binaryToCell char) row) (lines contents))
  print board

  iterateM_ $ do
    threadDelay 1000000
    --board <- return (Board.tick board)
    iterateM_ $
    print board
    return board
