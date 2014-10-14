import System.Environment
import System.IO
import qualified System.Console.ANSI as Console

import Control.Concurrent
import Control.Monad.Loops

import qualified Board as Board

binaryToCell '1' = Board.Full
binaryToCell '0' = Board.Empty

clearScreen = Console.clearScreen >> Console.setCursorPosition 0 0

tickBoard :: Board.Board -> IO Board.Board
tickBoard board =
  let ticked = Board.tick board
  in  threadDelay 1000000 >> clearScreen >> print ticked >> return ticked

main = do
  boardFile:_ <- getArgs
  handle <- openFile boardFile ReadMode
  contents <- hGetContents handle
  board <- return (Board.fromRowList $ map (\row -> map (\char -> binaryToCell char) row) (lines contents))
  clearScreen
  print board

  iterateM_ tickBoard board
