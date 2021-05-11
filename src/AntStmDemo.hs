{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

-- TODO: add tests for the exposed functions/types
-- TODO: add PBT to verify that for each step there's no more than 1 ant per cell and each ant is either where it started or on a cell adjacent to it.
-- TODO: allow neighboring ants to swap places when it's mutually beneficial
-- TODO: add an Async interface for the simulator

module AntStmDemo (someFunc, initAntGrid, report, showGrid, showCell, simulate, tryReadCell) where

import           AntStmDemo.Ant        (Ant (..), reachedDestination)
import           AntStmDemo.Coordinate (Coordinate (..), distanceBetween)
import           AntStmDemo.Grid       (Cell (..), Grid (..), initAntGrid,
                                        putCell, takeCell, tryReadCell)
import           AntStmDemo.Report     (report)
import           Control.Monad         (filterM, foldM)
import           Control.Monad.STM     (STM, orElse)
import           Data.List             (sortOn)
import qualified Data.Map              as Map

step :: Grid Cell -> Cell -> STM (Grid Cell)
step grid cell = do
  ant <- tryReadCell cell
  case ant of
    Nothing -> error "expected ant at cell"
    Just (Ant _ [] _) -> error "ant is missing an initial location"
    Just (Ant _ (location:_) destination) ->
      if location == destination
        then return grid
        else do
          let candidates = sortOn (distanceBetween destination) (adjacentCoords location)
          (newCellMap, _) <- foldM (tryMoveFrom location) (gridCell grid, False) candidates
          return (Grid w h newCellMap)
  where
    w = gridWidth grid
    h = gridHeight grid

    adjacentCoords (Coord x' y') =
      let
        neighbors = [Coord (x' + dx) (y' + dy) | dx <- [-1..1], dy <- [-1..1]]
        inBounds (Coord x'' y'') = and [x'' >= 0, x'' < w, y'' >= 0, y'' < h]
      in filter inBounds neighbors

    tryMoveFrom :: Coordinate -> (Map.Map Coordinate Cell, Bool) -> Coordinate -> STM (Map.Map Coordinate Cell, Bool)
    tryMoveFrom location (cellMap, done) to =
      if done
        then return (cellMap, done)
        else (moveAntFrom location cellMap to >>= \newCellMap -> return (newCellMap, True)) `orElse` return (cellMap, False)

    moveAntFrom :: Coordinate -> Map.Map Coordinate Cell -> Coordinate -> STM (Map.Map Coordinate Cell)
    moveAntFrom from cellMap to =
      case Map.lookup from cellMap of
        Nothing -> error "No ant to move from specified coordinate"
        Just fromCell -> do
          Ant ident travelLog dest <- takeCell fromCell
          case Map.lookup to cellMap of
            Nothing    -> error "Cell should have been initialized"
            Just toCell -> putCell toCell (Ant ident (to:travelLog) dest) >> return cellMap

simulate :: Grid Cell -> STM (Grid Cell)
simulate grid = do
  travelers <- activeCells grid
  if null travelers
    then return grid
    else foldM step grid travelers >>= simulate
  where
    activeCells :: Grid Cell -> STM [Cell]
    activeCells = filterM isActiveCell . Map.elems . gridCell

    isActiveCell :: Cell -> STM Bool
    isActiveCell = fmap (maybe False (not . reachedDestination)) . tryReadCell

showCell :: Cell -> STM String
showCell = fmap show . tryReadCell

showGrid :: Grid Cell -> STM String
showGrid = fmap show . mapM convertPair . Map.toList . gridCell
  where
    convertPair (coord, cell) = fmap (fmap (coord,)) (tryReadCell cell)

-- TODO: remove someFunc and tests that use it
someFunc :: IO ()
someFunc = putStrLn "someFunc"
