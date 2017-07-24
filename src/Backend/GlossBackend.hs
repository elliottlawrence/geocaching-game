{-# LANGUAGE TypeFamilies #-}
module Backend.GlossBackend where

import           Graphics.Gloss                     as Gloss
import           Graphics.Gloss.Data.ViewPort       as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import           Graphics.Gloss.Juicy

import           Constants
import           Types

data GlossBackend = GlossBackend

instance Backend GlossBackend where
  type FloatType GlossBackend = Float
  type Picture GlossBackend = Gloss.Picture
  type Color GlossBackend = Gloss.Color
  type EventT GlossBackend = Gloss.Event

  play _ initialGame renderGame handleInput updateGame =
    Gloss.play
      window
      Gloss.black
      60
      initialGame
      renderGame'
      (makeHandleInput handleInput)
      updateGame'
    where
      window = InWindow "Geocaching Game" (windowX, windowY) (200, 200)
      renderGame' = applyViewPortToPicture viewPort . renderGame
      viewPort = viewPortInit {
        viewPortTranslate = (-fromIntegral windowX/2, -fromIntegral windowY/2)
      }
      updateGame' _ = updateGame

  loadImage path = do
    png <- loadJuicyPNG path'
    case png of
      Just bmp@(Bitmap w h _ _) -> return $
        Translate (fromIntegral w/2) (fromIntegral h/2) bmp
      _ -> ioError $ userError $ "File not found: " ++ path'
    where path' = prefixPath path

  loadFile _ = readFile . prefixPath

  makeColor = makeColorI

  blank = Blank
  circleSolid = Gloss.circleSolid
  colored = Color
  line x1 y1 x2 y2 = Line [(x1,y1), (x2,y2)]
  pictures = Pictures
  polygon = Polygon
  text BigText = Scale 0.2 0.2 . Text
  text SmallText = Scale 0.15 0.15 . Text
  translate = Translate

  toEvent (Gloss.EventKey key keyState _ _) =
    maybe Nothing (\key' -> Just $ EventKey key' keyState') maybeKey
    where
      keyState' = case keyState of
        Gloss.Up -> Up
        Gloss.Down -> Down

      maybeKey = case key of
        Gloss.SpecialKey Gloss.KeyLeft -> Just KeyLeft
        Gloss.SpecialKey Gloss.KeyRight -> Just KeyRight
        Gloss.SpecialKey Gloss.KeyUp -> Just KeyUp
        Gloss.SpecialKey Gloss.KeyDown -> Just KeyDown
        Gloss.SpecialKey Gloss.KeySpace -> Just KeySpace
        Gloss.SpecialKey Gloss.KeyEnter -> Just KeyEnter
        Gloss.Char c -> Just (Char c)
        _ -> Nothing
  toEvent _ = Nothing


prefixPath :: FilePath -> FilePath
prefixPath = (++) "docs/"
