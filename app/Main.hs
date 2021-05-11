module Main where

import           AntStmDemo           (initAntGrid, report, simulate,
                                       tryReadCell)
import           AntStmDemo.Config    (fromArgs)
import           Control.Monad.STM    (atomically)
import           Data.Aeson           (ToJSON, encode)
import qualified Data.ByteString.Lazy as BS
import           System.Environment   (getArgs)
import           System.IO            (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case fmap initAntGrid (fromArgs args) of
    Nothing -> hPutStrLn stderr "ERROR: failed to initialize grid from provided arguments"
    Just grid -> do
      putStr "\nInitial Grid\n\t"
      atomically (grid >>= mapM tryReadCell) >>= printJSON
      putStrLn ""

      postSimGrid <- atomically (grid >>= simulate)

      putStr "Grid After Simulation\n\t"
      atomically (mapM tryReadCell postSimGrid) >>= printJSON
      putStrLn ""

      putStr "Simulation Report\n\t"
      atomically (report postSimGrid) >>= printJSON
      putStrLn ""
  where
    printJSON :: ToJSON a => a -> IO ()
    printJSON = BS.putStr . encode
