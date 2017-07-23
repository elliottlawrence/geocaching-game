module Image where

import           Data.Char
import           Data.Maybe

import           Backend
import           Types

loadPNG :: FilePath -> IO Picture
loadPNG path = do
  png <- loadJuicyPNG path
  case png of
    Just bmp@(Bitmap w h _ _) -> return $
      Translate (fromIntegral w/2) (fromIntegral h/2) bmp
    _ -> ioError $ userError $ "File not found: " ++ path

getPicNameForLevel :: Int -> PictureName
getPicNameForLevel i = toEnum $ fromEnum Level1Pic + i - 1

getPicPath :: PictureName -> FilePath
getPicPath picName = "docs/images/" ++ name ++ ".png"
  where constructorName = show picName
        name = map toLower
          $ take (length constructorName - length "Pic") constructorName

loadGetPic :: IO GetPic
loadGetPic = do
  let picNames = enumFrom (toEnum 0)
  picMap <- mapM (\picName -> do
    pic <- loadPNG (getPicPath picName)
    return (picName, pic))
    picNames
  return $ \picName -> fromJust (lookup picName picMap)
