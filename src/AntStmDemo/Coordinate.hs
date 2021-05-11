{-# LANGUAGE DeriveGeneric #-}

module AntStmDemo.Coordinate (Coordinate(..), distanceBetween, pickGridCoords) where

import           AntStmDemo.Config     (Config (..))
import           Data.Aeson            (ToJSON, ToJSONKey, defaultOptions,
                                        genericToEncoding, toEncoding)
import           GHC.Float             (int2Double)
import           GHC.Generics          (Generic)
import           System.Random         (RandomGen, split)
import           System.Random.Shuffle (shuffle')

data Coordinate
  = Coord
  { x :: Int
  , y :: Int
  }
  deriving (Generic, Show, Eq, Ord)

instance ToJSON Coordinate where
  toEncoding = genericToEncoding defaultOptions

instance ToJSONKey Coordinate where

distanceBetween :: Coordinate -> Coordinate -> Double
distanceBetween (Coord x1 y1) (Coord x2 y2) = (sqrt . int2Double) ((dx * dx) + (dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1

pickGridCoords :: RandomGen gen => Config -> gen -> ([Coordinate], gen)
pickGridCoords (Config w h n _) gen = (coords g1, g2)
  where
    (g1, g2) = split gen
    candidates = [Coord x' y' | x' <- [0..w-1], y' <- [0..h-1]]
    coords = take n . shuffle' candidates (w * h)
