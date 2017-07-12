module Utils where

import           Control.Monad.State
import           Graphics.Gloss
import           System.Random

import           Types

zipWith3M :: (Applicative m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f xs ys zs = sequenceA (zipWith3 f xs ys zs)

rectangle :: Int -> Int -> Picture
rectangle w h = Polygon [(0, 0), (w', 0), (w', h'), (0, h')]
  where [w', h'] = map fromIntegral [w, h]

randomToIO :: RandomT a -> IO a
randomToIO rand = do
  g <- newStdGen
  return $ evalState rand g

makeRandomT :: (StdGen -> a) -> RandomT a
makeRandomT f = do
  (g, g') <- split <$> get
  put g
  return $ f g'
