module Image where

import           Data.Char
import           Data.Maybe

import           Types

getPicNameForLevel :: Int -> PictureName
getPicNameForLevel i = toEnum $ fromEnum Level1Pic + i - 1

getPicPath :: PictureName -> FilePath
getPicPath picName = "docs/images/" ++ name ++ ".png"
  where constructorName = show picName
        name = map toLower
          $ take (length constructorName - length "Pic") constructorName

loadGetPic :: Backend a => IO (GetPic a)
loadGetPic = do
  let picNames = enumFrom (toEnum 0)
  picMap <- mapM (\picName -> do
    pic <- loadImage (getPicPath picName)
    return (picName, pic))
    picNames
  return $ \picName -> fromJust (lookup picName picMap)
