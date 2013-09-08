{-# OPTIONS_GHC -Wall #-}
module Sprite ( Sprite
              , loadSprite
              , drawSprite
              ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Graphics.UI.SDL
import Graphics.UI.SDL.Image (load)

import GLFWHelpers

type Sprite = Surface

loadSprite :: FilePath -> IO (Sprite)
loadSprite fp = do
  surf <- load fp
  return surf

--  im <- loadImageOrError fp
--  tex <- makeSimpleBitmapTexture im
--  return (tex, bitmapSize im)

--loadImageOrError :: FilePath -> IO (Image)
--loadImageOrError fp = undefined do
--  image <- loadImage fp
--  case image of
--    Left _ -> error "Failed to load sprite image"
--    Right im -> return im

drawSprite :: Sprite -> (Int, Int) -> Drawing ()
drawSprite im (px, py) = do
  tgt <- asks surface
  let w = surfaceGetWidth im
      h = surfaceGetHeight im
  _ <- liftIO $ blitSurface im Nothing tgt $ Just $ Rect px py w h
  return ()

--  activeTexture $= TextureUnit 0
--  textureBinding Texture2D $= Just tex
--  renderPrimitive Quads $ mapM_ texVert quad
--  where
--    tex = fst sprite
--    quad :: [(GLfloat, GLfloat, GLfloat, GLfloat)]
--    quad = [(px, py, 0, 0),
--            (px+w, py, 1, 0),
--            (px+w, py+h, 1, 1),
--            (px, py+h, 0, 1)]

--texVert :: (TexCoordComponent a, VertexComponent b) => (a, a, b, b) -> IO ()
--texVert (s, t, x, y) = do
--  texCoord $ TexCoord2 s t
--  vertex $ Vertex2 x y
