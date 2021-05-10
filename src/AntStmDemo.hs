{-# LANGUAGE FlexibleInstances #-}

-- TODO: add tests for the exposed functions/types
-- TODO: add PBT to verify that for each step there's no more than 1 ant per cell and each ant is either where it started or on a cell adjacent to it.
-- TODO: allow neighboring ants to swap places when it's mutually beneficial
-- TODO: add an Async interface for the simulator

module AntStmDemo (someFunc, initAntGrid, showGrid, activeCells, showCell, simulate, report) where

import           AntStmDemo.Ant               (Ant (..), reachedDestination)
import           AntStmDemo.Coordinate        (Coordinate (..), adjacentCoords,
                                               distanceBetween)
import           AntStmDemo.Grid              (Cell (..), Grid (..),
                                               initAntGrid, tryReadCell)
import           AntStmDemo.Report            (report)
import           Control.Concurrent.STM.TMVar (putTMVar, takeTMVar,
                                               tryReadTMVar)
import           Control.Monad                (filterM, foldM)
import           Control.Monad.STM            (STM, orElse)
import           Data.List                    (sortOn)
import qualified Data.Map                     as Map

step :: Grid -> Cell -> STM Grid
step grid cell = do
  ant <- tryReadCell cell
  case ant of
    Nothing -> error "expected ant at cell"
    Just (Ant _ [] _) -> error "ant is missing an initial location"
    Just (Ant _ (location:_) destination) ->
      if location == destination
        then return grid
        else do
          let candidates = sortOn (distanceBetween destination) (adjacentCoords w h location)
          (newCellMap, _) <- foldM (tryMoveFrom location) (gridCell grid, False) candidates
          return (Grid w h newCellMap)
  where
    w = gridWidth grid
    h = gridHeight grid

    moveAntFrom :: Coordinate -> Map.Map Coordinate Cell -> Coordinate -> STM (Map.Map Coordinate Cell)
    moveAntFrom from cellMap to =
      case fmap unCell (Map.lookup from cellMap) of
        Nothing -> error "No ant to move from specified coordinate"
        Just fromVar -> do
          Ant ident travelLog dest <- takeTMVar fromVar
          let newAnt = Ant ident (to:travelLog) dest
          case fmap unCell (Map.lookup to cellMap) of
            Nothing    -> error "Cell should have been initialized"
            Just toVar -> putTMVar toVar newAnt >> return cellMap

    tryMoveFrom :: Coordinate -> (Map.Map Coordinate Cell, Bool) -> Coordinate -> STM (Map.Map Coordinate Cell, Bool)
    tryMoveFrom location (cellMap, done) to =
      if done
        then return (cellMap, done)
        else (moveAntFrom location cellMap to >>= \newCellMap -> return (newCellMap, True)) `orElse` return (cellMap, False)

isActiveCell :: Cell -> STM Bool
isActiveCell = fmap (maybe False (not . reachedDestination)) . tryReadTMVar . unCell

activeCells :: Grid -> STM [Cell]
activeCells = filterM isActiveCell . Map.elems . gridCell

simulate :: Grid -> STM Grid
simulate grid = do
  travelers <- activeCells grid
  if null travelers
    then return grid
    else do
      newGrid <- foldM step grid travelers
      simulate newGrid

showCell :: Cell -> STM String
showCell = fmap show . tryReadCell

showGrid :: Grid -> STM String
showGrid grid = do
  cells <- mapM (\(coord, cell) -> tryReadCell cell >>= \c -> return (coord, c)) (Map.toList (gridCell grid))
  return (show cells)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
