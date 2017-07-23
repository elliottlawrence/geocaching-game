module Backend.ShineBackend where

import           GHCJS.DOM                      (currentDocumentUnchecked)
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           Graphics.Shine

import           Types

data ShineBackend = ShineBackend

instance Backend ShineBackend where
