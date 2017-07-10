module Utils where

import           Graphics.Gloss

zipWith3M :: (Applicative m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f xs ys zs = sequenceA (zipWith3 f xs ys zs)

rectangle :: Int -> Int -> Picture
rectangle w h = Polygon [(0, 0), (w', 0), (w', h'), (0, h')]
  where [w', h'] = map fromIntegral [w, h]
