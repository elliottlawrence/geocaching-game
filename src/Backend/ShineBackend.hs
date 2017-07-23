{-# LANGUAGE TypeFamilies #-}
module Backend.ShineBackend where

-- import           GHCJS.DOM                      (currentDocumentUnchecked)
-- import           GHCJS.DOM.NonElementParentNode (getElementById)
import qualified Graphics.Shine         as Shine
import qualified Graphics.Shine.Picture as Shine

import           Types

data ShineBackend = ShineBackend

instance Backend ShineBackend where
  type FloatType ShineBackend = Double
  type Picture ShineBackend = Shine.Picture
  type Color ShineBackend = Shine.Color
