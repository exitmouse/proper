{-# OPTIONS_GHC -Wall #-}
module Sprite ( Sprite
              , loadSprite
              ) where

import Data.Bitmap.Base (Size, bitmapSize)
import Data.Bitmap.OpenGL (makeSimpleBitmapTexture)
import Codec.Image.STB (Image, loadImage)
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (TextureObject)

type Sprite = (TextureObject, Size)

loadSprite :: FilePath -> IO (Sprite)
loadSprite fp = do
  im <- loadImageOrError fp
  tex <- makeSimpleBitmapTexture im
  return (tex, bitmapSize im)

loadImageOrError :: FilePath -> IO (Image)
loadImageOrError fp =
  do
    image <- loadImage fp
    case image of
      Left _ -> error "Failed to load sprite image"
      Right im -> return im
