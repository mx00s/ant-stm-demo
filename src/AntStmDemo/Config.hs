module AntStmDemo.Config (Config(..), mkConfig, fromArgs) where

import           Text.Read (readMaybe)

data Config
  = Config
  { cfgWidth   :: Int
  , cfgHeight  :: Int
  , cfgNumAnts :: Int
  , cfgRngSeed :: Maybe Int
  }
  deriving Show

mkConfig :: Int -> Int -> Int -> Maybe Int -> Maybe Config
mkConfig w h n seed =
  if all (>= 0) [w, h, n] && n <= w * h then
    Just (Config w h n seed)
  else
    Nothing

fromArgs :: [String] -> Maybe Config
fromArgs args =
  case mapM readMaybe args of
    Just [w,h,n,s] -> mkConfig w h n (Just s)
    Just [w,h,n]   -> mkConfig w h n Nothing
    _              -> Nothing
