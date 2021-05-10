{-# LANGUAGE DeriveGeneric #-}

module AntStmDemo.Coordinate (Coordinate(..), distanceBetween, cellCoords, adjacentCoords, pickGridCoords) where

import           AntStmDemo.Config     (Config (..))
import           Data.Aeson            (ToJSON, ToJSONKey, defaultOptions,
                                        genericToEncoding, toEncoding)
import           GHC.Float             (int2Double)
import           GHC.Generics          (Generic)
import           System.Random         (RandomGen, split)
import           System.Random.Shuffle (shuffle')

data Coordinate = Coord Int Int
  deriving (Generic, Eq, Ord)

instance Show Coordinate where
  show (Coord x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance ToJSON Coordinate where
  toEncoding = genericToEncoding defaultOptions

instance ToJSONKey Coordinate where

distanceBetween :: Coordinate -> Coordinate -> Double
distanceBetween (Coord x1 y1) (Coord x2 y2) = (sqrt . int2Double) ((dx * dx) + (dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1

cellCoords :: Int -> Int -> [Coordinate]
cellCoords w h = [Coord x y | x <- [0..w-1], y <- [0..h-1]]

inBounds :: Int -> Int -> Coordinate -> Bool
inBounds w h (Coord x y) = and [x >= 0, x < w, y >= 0, y < h]

adjacentCoords :: Int -> Int -> Coordinate -> [Coordinate]
adjacentCoords w h (Coord x y) = filter (inBounds w h) [Coord (x + dx) (y + dy) | dx <- [-1..1], dy <- [-1..1]]

pickGridCoords :: RandomGen gen => Config -> gen -> ([Coordinate], gen)
pickGridCoords (Config w h n _) gen = (coords g1, g2)
  where
    (g1, g2) = split gen
    coords = take n . shuffle' (cellCoords w h) (w * h)
