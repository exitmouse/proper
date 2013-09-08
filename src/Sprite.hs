{-# OPTIONS_GHC -Wall #-}
module Sprite ( Sprite
              , loadSprite
              , drawSprite
              ) where

import Data.Bitmap.Base (bitmapSize)
import Data.Bitmap.OpenGL (makeSimpleBitmapTexture)
import Codec.Image.STB (Image, loadImage)
import Graphics.Rendering.OpenGL

type Sprite = (TextureObject, (Int, Int))

loadSprite :: FilePath -> IO (Sprite)
loadSprite fp = do
  im <- loadImageOrError fp
  tex <- makeSimpleBitmapTexture im
  return (tex, bitmapSize im)

loadImageOrError :: FilePath -> IO (Image)
loadImageOrError fp = do
  image <- loadImage fp
  case image of
    Left _ -> error "Failed to load sprite image"
    Right im -> return im

drawSprite :: Sprite -> (GLfloat, GLfloat) -> IO ()
drawSprite sprite (px, py) = do
  activeTexture $= TextureUnit 0
  textureBinding Texture2D $= Just tex
  renderPrimitive Quads $ mapM_ texVert quad
  where
    tex = fst sprite
    quad :: [(GLfloat, GLfloat, GLfloat, GLfloat)]
    quad = [(px, py, 0, 0),
            (px+w, py, 1, 0),
            (px+w, py+h, 1, 1),
            (px, py+h, 0, 1)]
    w = fromIntegral $ fst $ snd sprite
    h = fromIntegral $ snd $ snd sprite

texVert :: (TexCoordComponent a, VertexComponent b) => (a, a, b, b) -> IO ()
texVert (s, t, x, y) = do
  texCoord $ TexCoord2 s t
  vertex $ Vertex2 x y
