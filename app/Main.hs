module Main where

import           AntStmDemo         (activeCells, initAntGrid, report, showCell,
                                     showGrid, simulate)
import           AntStmDemo.Config  (fromArgs)
import           Control.Monad.STM  (atomically)
import           Data.Aeson         (encode)
import           System.Environment (getArgs)
import           System.IO          (hPutStrLn, stderr)

-- TODO: CLI: emit initial grid and report as JSON

main :: IO ()
main = do
  args <- getArgs
  case fmap initAntGrid (fromArgs args) of
    Nothing -> hPutStrLn stderr "ERROR: failed to initialize grid from provided arguments"
    Just grid -> do
      putStr "\nInitial Grid\n\t"
      atomically (grid >>= showGrid) >>= (print . encode)
      -- print (encode grid)

      putStr "Active Cells\n\t"
      atomically (grid >>= activeCells >>= mapM showCell) >>= print

      postSimGrid <- atomically (grid >>= simulate)

      putStr "\nGrid After Simulation\n\t"
      atomically (showGrid postSimGrid) >>= putStrLn

      putStr "Active Cells After Simulation\n\t"
      atomically (activeCells postSimGrid >>= mapM showCell) >>= print

      putStr "Simulation Report\n\t"
      atomically (report postSimGrid) >>= (print . encode)
