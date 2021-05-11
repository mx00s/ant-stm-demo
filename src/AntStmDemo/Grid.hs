{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module AntStmDemo.Grid (Cell(..), Grid(..), mkAntCell, initCellMap, initAntGrid, putCell, takeCell, tryReadCell) where

import           AntStmDemo.Ant               (Ant, AntId (..), mkAnt)
import           AntStmDemo.Config            (Config (..))
import           AntStmDemo.Coordinate        (Coordinate (..), pickGridCoords)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, newTMVar,
                                               putTMVar, takeTMVar,
                                               tryReadTMVar)
import           Control.Monad.STM            (STM)
import           Data.Aeson                   (ToJSON, defaultOptions,
                                               genericToEncoding, toEncoding)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           System.Random                (mkStdGen)

newtype Cell = Cell { unCell :: TMVar Ant }
  deriving Generic

mkAntCell :: Ant -> STM Cell
mkAntCell ant = fmap Cell (newTMVar ant)

data Grid a
  = Grid
  { gridWidth  :: Int
  , gridHeight :: Int
  , gridCell   :: Map.Map Coordinate a
  }
  deriving (Generic, Functor, Foldable)

deriving instance Traversable Grid

instance ToJSON (Grid (Maybe Ant)) where
  toEncoding = genericToEncoding defaultOptions

takeCell :: Cell -> STM Ant
takeCell = takeTMVar . unCell

putCell :: Cell -> Ant -> STM ()
putCell cell = putTMVar (unCell cell)

tryReadCell :: Cell -> STM (Maybe Ant)
tryReadCell = tryReadTMVar . unCell

toCell :: Maybe Ant -> STM Cell
toCell = maybe (fmap Cell newEmptyTMVar) mkAntCell

initCellMap :: Config -> STM (Map.Map Coordinate Cell)
initCellMap config = mapM toCell cellMap
  where
    w = cfgWidth config
    h = cfgHeight config
    fromToCoords gen = let
      (froms, gen2) = pickGridCoords config gen
      (tos, _) = pickGridCoords config gen2
      in zip froms tos
    defaultSeed = 0
    fromsAndTos = (fromToCoords . mkStdGen . fromMaybe defaultSeed . cfgRngSeed) config
    blankCells = [(Coord x' y', Nothing) | x' <- [0..w-1], y' <- [0..h-1]]
    antCells = [(from, Just (mkAnt ident from to)) | (ident, (from, to)) <- zip (fmap AntId [0..]) fromsAndTos]
    cellMap = Map.fromList (blankCells ++ antCells)

initAntGrid :: Config -> STM (Grid Cell)
initAntGrid config = fmap (Grid w h) (initCellMap config)
  where
    w = cfgWidth config
    h = cfgHeight config
