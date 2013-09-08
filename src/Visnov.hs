{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Visnov ( Visnov
              , Dialogue
              , runVisnov
              , getCharacter
              , getChoice
              , pose
              , as
              , say
              , background
              ) where

import Control.Monad (Monad, (>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, Reader(..), runReader, ReaderT(..), runReaderT, ask, asks, local)
import Control.Monad.State (StateT, runStateT, get, put)
import qualified Data.Map as M
import Data.String (IsString, fromString)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW

import VisnovDesc
import Sprite (drawSprite)
import GLFWHelpers

type Visnov s = ReaderT World (StateT s IO)
type Event s = Visnov s ()

newtype Dialogue s a = Dialogue { unDialogue :: (ReaderT Character (Visnov s) a)} deriving (Monad)
instance IsString (Dialogue s ()) where
  fromString s = Dialogue $ liftIO (writeGameText s)

runVisnov :: Visnov s a -> World -> s -> IO (a, s)
runVisnov v w s = do
  let width  = 640
      height = 480

  withWindow width height "GLFW-b-demo" $ \win -> do
    GLFW.swapInterval 1
    --GL.enable CapDepthTest Disabled 
    --GL.enable CapAlphaTest Disabled 
    --GL.enable CapBlend Enabled
    GL.clearColor GL.$= GL.Color4 0.5 0.5 0.5 1
    --GL.depthTest GL.$= GL.Disabled
    --GL.alphaTest GL.$= GL.Disabled
    GL.dither GL.$= GL.Disabled
    GL.blend GL.$= GL.Enabled
    GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    --let env = Env
    --    { envEventsChan    = eventsChan
    --    , envWindow        = win
    --    }
    --  state = State
    --    { stateWindowWidth     = width
    --    , stateWindowHeight    = height
    --    }
    --runDemo env state
    runStateT (runReaderT v w) s

as :: Character -> Dialogue s () -> Visnov s ()
as = runDialogueWith

say :: Dialogue s () -> Dialogue s ()
say = id

runDialogueWith :: Character -> Dialogue s a -> Visnov s a
runDialogueWith p d = runReaderT (unDialogue d) p

getCharacter :: (MonadReader World m)
          => String
          -> m (Character)
getCharacter s = do
  m_character <- asks $ (M.lookup s) . worldCharacterMap
  case m_character of
    Nothing -> error "Requested character does not exist"
    Just character -> return character

background :: BackgroundID -> Event u
background b = do
  m_bg <- asks $ (M.lookup b) . worldBackgroundMap
  case m_bg of
    Nothing -> error "Requested background does not exist"
    Just bg -> drawBg bg

-- Requires IO, so isn't more general
getChoice :: [(String, Visnov s a)] -> Visnov s a
getChoice [] = error "Call getChoice with a larger list"
getChoice (x:xs) = snd x -- Choose the first one TODO

pose :: String -> Dialogue u ()
pose s = Dialogue $ do
  character <- ask
  let m_frame = M.lookup s (characterFrames character)
  case m_frame of
    Nothing -> error "No such pose"
    Just frame -> ReaderT $ \r -> (drawChar frame :: Event u)

drawChar :: Pose -> Event u
drawChar f = liftIO $ drawSprite f (0, 0)
drawBg :: Background -> Event u
drawBg f = liftIO $ drawSprite f (0, 0)

writeGameText :: String -> IO ()
writeGameText = liftIO . putStrLn
