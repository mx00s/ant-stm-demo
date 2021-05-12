module AntStmDemo.Config (Config(..), mkConfig, fromArgs) where

import           Control.Monad.Random.Lazy (MonadRandom, getRandom)
import           Text.Read                 (readMaybe)

data Config
  = Config
  { cfgWidth   :: Int
  , cfgHeight  :: Int
  , cfgNumAnts :: Int
  , cfgRngSeed :: Int
  }
  deriving Show

mkConfig :: Int -> Int -> Int -> Int -> Maybe Config
mkConfig w h n s =
  if all (>= 0) [w, h, n] && n <= w * h then
    Just (Config w h n s)
  else
    Nothing

fromArgs :: MonadRandom m => [String] -> m (Maybe Config)
fromArgs args =
  case mapM readMaybe args of
    Just [w,h,n,s] -> return (mkConfig w h n s)
    Just [w,h,n]   -> fmap (mkConfig w h n) getRandom
    _              -> return Nothing
