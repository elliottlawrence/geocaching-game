{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Renderable where

import           Types

class Backend b => Renderable a b where
  render :: a -> Picture b

instance Renderable a b => Renderable [a] b where
  render = pictures . map render
