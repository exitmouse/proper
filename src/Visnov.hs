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

import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad (Monad, void, (>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, Reader(..), runReader, ReaderT(..), runReaderT, ask, asks, local)
import Control.Monad.RWS.Strict (evalRWST)
import Control.Monad.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as M
import Data.String (IsString, fromString)
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image (load)

import VisnovDesc
import Sprite (drawSprite)
import GLFWHelpers


type Visnov s = ReaderT World (StateT s Drawing)
type Event s = Visnov s ()

newtype Dialogue s a = Dialogue { unDialogue :: (ReaderT Character (Visnov s) a)} deriving (Monad)
instance IsString (Dialogue s ()) where
  fromString s = (writeGameText s)

runVisnov :: Visnov s a -> World -> s -> IO ()
runVisnov v w s = do
  let width  = 640
      height = 480
  SDL.withInit [SDL.InitEverything] $ do
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    --hello <- load "img/cat.png"
    --SDL.blitSurface hello Nothing screen Nothing
    SDL.flip screen
    let env = Env
          { surface = screen
          }
        state = State
          { stateWindowWidth     = width
          , stateWindowHeight    = height
          , advanceText          = False
          }
    void $ evalRWST (runStateT (runReaderT v w) s) env state

as :: Character -> Dialogue s () -> Visnov s ()
as = runDialogueWith

promote :: Drawing a -> Visnov u a
promote = lift . lift

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
    Just frame -> lift (drawChar frame :: Event u)

drawChar :: Pose -> Event u
drawChar f = promote $ drawSprite f (0, 0)
drawBg :: Background -> Event u
drawBg f = promote $ drawSprite f (0, 0)

writeGameText :: String -> Dialogue u ()
writeGameText s = Dialogue $ do
  (Character cname _) <- ask
  lift $ writeGameTextByChar cname s

writeGameTextByChar :: String -> String -> Visnov u ()
writeGameTextByChar cname s = promote $ drawGameText (cname ++ ": " ++ s)
