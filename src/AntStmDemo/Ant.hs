{-# LANGUAGE DeriveGeneric #-}

module AntStmDemo.Ant (AntId(..), Ant(..), mkAnt, reachedDestination) where

import           AntStmDemo.Coordinate (Coordinate)
import           Data.Aeson            (ToJSON, ToJSONKey,
                                        ToJSONKeyFunction (..), defaultOptions,
                                        encode, fromEncoding, genericToEncoding,
                                        toEncoding, toJSONKey)
import           Data.Text             (pack)
import           GHC.Generics          (Generic)

newtype AntId = AntId Int
  deriving (Generic, Show, Eq, Ord)

instance ToJSON AntId where
  toEncoding = genericToEncoding defaultOptions

-- TODO: add a manual override for `toJSONKey` so the report rendered
-- as JSON returns an object instead of an array
instance ToJSONKey AntId where
  -- toJSONKey = ToJSONKeyText (\k -> pack ("ant" ++ show k)) _

data Ant
  = Ant
  { antId          :: AntId
  , antTravelLog   :: [Coordinate]
  , antDestination :: Coordinate
  }
  deriving Show

mkAnt :: AntId -> Coordinate -> Coordinate -> Ant
mkAnt ident from = Ant ident [from]

reachedDestination :: Ant -> Bool
reachedDestination (Ant _ travelLog destination) =
  case travelLog of
    []           -> False
    (location:_) -> location == destination
