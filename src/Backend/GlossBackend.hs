{-# LANGUAGE TypeFamilies #-}
module Backend.GlossBackend where

import qualified Graphics.Gloss                     as Gloss
import qualified Graphics.Gloss.Data.ViewPort       as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Gloss
import           Graphics.Gloss.Juicy

import           Constants
import           Types

data GlossBackend = GlossBackend

instance Backend GlossBackend where
  type FloatType GlossBackend = Float
  type Picture GlossBackend = Gloss.Picture
  type Color GlossBackend = Gloss.Color

  play _ fps initialGame renderGame handleInput updateGame =
    Gloss.play
      window
      Gloss.black
      fps
      initialGame
      renderGame'
      handleInput'
      updateGame'
    where
      window = Gloss.InWindow "Geocaching Game" (windowX, windowY) (200, 200)
      renderGame' = Gloss.applyViewPortToPicture viewPort . renderGame
      viewPort = Gloss.viewPortInit {
        Gloss.viewPortTranslate = (-fromIntegral windowX/2, -fromIntegral windowY/2)
      }
      handleInput' event game = maybe game (`handleInput` game) (toEvent event)
      updateGame' _ = updateGame

  loadImage path = do
    png <- loadJuicyPNG path
    case png of
      Just bmp@(Gloss.Bitmap w h _ _) -> return $
        Gloss.Translate (fromIntegral w/2) (fromIntegral h/2) bmp
      _ -> ioError $ userError $ "File not found: " ++ path

  makeColor = Gloss.makeColorI

  blank = Gloss.Blank
  circleSolid = Gloss.circleSolid
  colored = Gloss.Color
  line = Gloss.Line
  pictures = Gloss.Pictures
  polygon = Gloss.Polygon
  scale = Gloss.Scale
  text = Gloss.Text
  translate = Gloss.Translate

toEvent :: Gloss.Event -> Maybe Event
toEvent (Gloss.EventKey key keyState _ _) =
  maybe Nothing (\key' -> Just $ EventKey key' (toKeyState keyState)) maybeKey
  where
    toKeyState Gloss.Up = Up
    toKeyState Gloss.Down = Down

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
