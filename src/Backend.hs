module Backend (
  Gloss.Picture(..),
  Gloss.Color,
  Gloss.Event(..),
  Gloss.Key(..),
  Gloss.KeyState(..),
  Gloss.SpecialKey(..),
  getWindow,
  Gloss.circleSolid,
  Gloss.black,
  Gloss.play,
  Gloss.pictures,
  Gloss.makeColorI,
  Gloss.viewPortTranslate,
  Gloss.viewPortInit,
  Gloss.applyViewPortToPicture,
  Juicy.loadJuicyPNG,
) where

import qualified Graphics.Gloss                     as Gloss
import qualified Graphics.Gloss.Data.ViewPort       as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import qualified Graphics.Gloss.Juicy               as Juicy

getWindow :: String -> (Int, Int) -> (Int, Int) -> Gloss.Display
getWindow = Gloss.InWindow
