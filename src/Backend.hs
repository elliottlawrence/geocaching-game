{-# LANGUAGE CPP #-}
module Backend where

#ifdef __GHCJS__

import           Backend.ShineBackend

defaultBackend :: ShineBackend
defaultBackend = ShineBackend

#else

import           Backend.GlossBackend

defaultBackend :: GlossBackend
defaultBackend = GlossBackend

#endif
