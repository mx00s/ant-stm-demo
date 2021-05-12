{-# LANGUAGE LambdaCase #-}

module Main where

import           AntStmDemo           (initAntGrid, report, simulate,
                                       tryReadCell)
import           AntStmDemo.Config    (cfgRngSeed, fromArgs)
import           Control.Monad.STM    (atomically)
import           Data.Aeson           (ToJSON, encode)
import qualified Data.ByteString.Lazy as BS
import           System.Environment   (getArgs)
import           System.IO            (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= fromArgs >>= \case
  Nothing -> hPutStrLn stderr "ERROR: failed to initialize grid from provided arguments"
  Just config -> do
    putStrLn ("Using seed: " ++ show (cfgRngSeed config))

    grid <- atomically (initAntGrid config)
    putStr "\nInitial Grid\n\t"
    atomically (mapM tryReadCell grid) >>= printJSON
    putStrLn ""

    postSimGrid <- atomically (simulate grid)

    putStr "Grid After Simulation\n\t"
    atomically (mapM tryReadCell postSimGrid) >>= printJSON
    putStrLn ""

    putStr "Simulation Report\n\t"
    atomically (report postSimGrid) >>= printJSON
    putStrLn ""
  where
    printJSON :: ToJSON a => a -> IO ()
    printJSON = BS.putStr . encode
