module Renderable where

import           Graphics.Gloss

class Renderable a where
  render :: a -> Picture

instance Renderable a => Renderable [a] where
  render = pictures . map render