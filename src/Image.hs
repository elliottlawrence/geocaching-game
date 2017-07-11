module Image where

import           Graphics.Gloss
import           Graphics.Gloss.Juicy

loadPNG :: FilePath -> IO Picture
loadPNG path = do
  png <- loadJuicyPNG path
  case png of
    Just bmp@(Bitmap w h _ _) -> return $
      Translate (fromIntegral w/2) (fromIntegral h/2) bmp
    _ -> ioError $ userError $ "File not found: " ++ path
