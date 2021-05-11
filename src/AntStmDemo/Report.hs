{-# LANGUAGE DeriveGeneric #-}

module AntStmDemo.Report (Report(..), report) where

import           AntStmDemo.Ant        (Ant (..), AntId)
import           AntStmDemo.Coordinate (Coordinate)
import           AntStmDemo.Grid       (Cell, Grid (..), tryReadCell)
import           Control.Monad.STM     (STM)
import           Data.Aeson            (ToJSON, defaultOptions,
                                        genericToEncoding, toEncoding)
import           Data.List             (sortOn)
import qualified Data.Map              as Map
import           Data.Maybe            (isJust)
import           GHC.Generics          (Generic)

-- newtype Report = Report [(AntId, [Coordinate])]
newtype Report = Report (Map.Map AntId [Coordinate])
  deriving (Generic, Show)

instance ToJSON Report where
  toEncoding = genericToEncoding defaultOptions

report :: Grid Cell -> STM Report
report grid = do
  cells <- fmap (filter isJust) (mapM tryReadCell (Map.elems (gridCell grid)))
  (return . Report . Map.fromList . sortOn fst . fmap antReport) cells
  where
    antReport (Just (Ant ident travelLog _)) = (ident, reverse travelLog)
    antReport Nothing                        = error "unreachable"
