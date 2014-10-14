import System.Environment
import System.IO
import Control.Concurrent
import Control.Monad
import qualified Board as Board

binaryToCell '1' = Board.Full
binaryToCell '0' = Board.Empty

main = do
  boardFile:_ <- getArgs
  handle <- openFile boardFile ReadMode
  contents <- hGetContents handle
  board <- return (Board.fromRowList $ map (\row -> map (\char -> binaryToCell char) row) (lines contents))
  print board

  forever $ do
    threadDelay 10000
    board <- return (Board.tick board)
    print board
