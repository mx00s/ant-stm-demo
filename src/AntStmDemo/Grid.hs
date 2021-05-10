{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module AntStmDemo.Grid (Cell(..), Grid(..), mkAntCell, initCellMap, initAntGrid, tryReadCell) where

import           AntStmDemo.Ant               (Ant, AntId (..), mkAnt)
import           AntStmDemo.Config            (Config (..))
import           AntStmDemo.Coordinate        (Coordinate (..), pickGridCoords)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar, newTMVar,
                                               tryReadTMVar)
import           Control.Monad.STM            (STM)
import           Data.Aeson                   (ToJSON, ToJSONKey,
                                               ToJSONKeyFunction (..),
                                               defaultOptions, encode,
                                               fromEncoding, genericToEncoding,
                                               toEncoding, toJSONKey)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           GHC.Generics                 (Generic)
import           System.Random                (mkStdGen)

newtype Cell = Cell { unCell :: TMVar Ant }
  deriving Generic

-- instance ToJSON Cell where
--   toEncoding = genericToEncoding defaultOptions

tryReadCell :: Cell -> STM (Maybe Ant)
tryReadCell = tryReadTMVar . unCell

mkAntCell :: Ant -> STM Cell
mkAntCell ant = fmap Cell (newTMVar ant)

-- TODO: Parameterize Grid's cell type and derive Functor so Cells can
-- easily be converted between `Cell` and `Maybe Ant`
-- representations. Helps with rendering Grids as JSON because there's
-- no sensible way to implement ToJSON on Cell.
data Grid
  = Grid
  { gridWidth  :: Int
  , gridHeight :: Int
  , gridCell   :: Map.Map Coordinate Cell
  }
  deriving Generic

-- instance ToJSON Grid where
--   toEncoding = genericToEncoding defaultOptions

initCellMap :: Config -> STM [(Coordinate, Cell)]
initCellMap config = sequence cellMap
  where
    w = cfgWidth config
    h = cfgHeight config
    fromToCoords gen = let
      (froms, gen2) = pickGridCoords config gen
      (tos, _) = pickGridCoords config gen2
      in zip froms tos
    defaultSeed = 0
    fromsAndTos = (fromToCoords . mkStdGen . fromMaybe defaultSeed . cfgRngSeed) config
    blankCells = [newEmptyTMVar >>= \var -> return (Coord x y, Cell var) | x <- [0..(w - 1)], y <- [0..(h - 1)]]
    cellMap = blankCells ++ [fmap (from,) (mkAntCell (mkAnt ident from to)) | (ident, (from, to)) <- zip (fmap AntId [0..]) fromsAndTos]

initAntGrid :: Config -> STM Grid
initAntGrid config = fmap (Grid w h . Map.fromList) (initCellMap config)
  where
    w = cfgWidth config
    h = cfgHeight config
