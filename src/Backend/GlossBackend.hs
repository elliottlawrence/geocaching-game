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
  type Picture GlossBackend = Gloss.Picture

  loadImage path = do
    png <- loadJuicyPNG path
    case png of
      Just bmp@(Gloss.Bitmap w h _ _) -> return $
        Gloss.Translate (fromIntegral w/2) (fromIntegral h/2) bmp
      _ -> ioError $ userError $ "File not found: " ++ path

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
      handleInput' event game = case toEvent event of
        Just event' -> handleInput event' game
        Nothing -> game
      updateGame' _ = updateGame

  blank = Gloss.Blank
  circleSolid = Gloss.circleSolid . realToFrac
  colored (Color r g b a) = Gloss.Color (Gloss.makeColorI r g b a)
  line = Gloss.Line . map toFloat
  pictures = Gloss.Pictures
  polygon = Gloss.Polygon . map toFloat
  scale x y = Gloss.Scale (realToFrac x) (realToFrac y)
  text = Gloss.Text
  translate x y = Gloss.Translate (realToFrac x) (realToFrac y)

toFloat :: (Double, Double) -> (Float, Float)
toFloat (x,y) = (realToFrac x, realToFrac y)

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
