module Utils where

zipWith3M :: (Applicative m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f xs ys zs = sequenceA (zipWith3 f xs ys zs)
