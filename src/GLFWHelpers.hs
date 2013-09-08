module GLFWHelpers ( Drawing
                   , Env(..)
                   , State(..)
                   , blitGameText
                   , blitGameTextBox
                   , writeGameTextByChar
                   , writeMenuText
                   , waitForTextAdvance
                   , waitForMenuChoice
                   ) where

--------------------------------------------------------------------------------

import Prelude hiding (flip)
import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Monad             (unless, when, void)
--import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes)

import Graphics.UI.SDL
import Graphics.UI.SDL.TTF
import Graphics.UI.SDL.Primitives (box)

--------------------------------------------------------------------------------

data Env = Env
    { surface :: Surface
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , currentText          :: !String
    , currentPose          :: !String
    , currentBg            :: !String
    }

--------------------------------------------------------------------------------

type Drawing = RWST Env () State IO

writeGameTextByChar :: String -> String -> Drawing ()
writeGameTextByChar cname s = do
  blitGameTextBox
  blitGameText (cname ++ ": " ++ s)

writeMenuText :: String -> Drawing ()
writeMenuText s = do
  blitMenuTextBox
  blitMenuText s

-- Blit functions don't flip. Draw functions do.
blitGameTextBox :: Drawing ()
blitGameTextBox = do
  tgt <- asks surface
  let x1 = 35
      y1 = 480
      x2 = 605
      y2 = 295
  _ <- liftIO $ box tgt (Rect x1 y1 x2 y2) (Pixel 0xffbe6cbb)
  return ()

blitMenuTextBox :: Drawing ()
blitMenuTextBox = do
  tgt <- asks surface
  let x1 = 35
      y1 = 445
      x2 = 605
      y2 = 35
  _ <- liftIO $ box tgt (Rect x1 y1 x2 y2) (Pixel 0xffbe6cbb)
  return ()

blitText :: Int -> Int -> String -> Drawing ()
blitText x y s = do
  tgt <- asks surface
  modify $ \state -> state { currentText = s }
  font <- liftIO $ openFont "/usr/share/fonts/TTF/LiberationMono-Regular.ttf" 12
  im <- liftIO $ renderUTF8Solid font s (Color 0 0 255)
  let w = surfaceGetWidth im
      h = surfaceGetHeight im
  _ <- liftIO $ blitSurface im Nothing tgt $ Just $ Rect x y w h
  liftIO $ flip tgt
  liftIO $ delay 2000
  return ()

blitMenuText :: String -> Drawing ()
blitMenuText = blitText 40 40

blitGameText :: String -> Drawing ()
blitGameText = blitText 40 300

waitForTextAdvance :: Drawing ()
waitForTextAdvance = do
 e <- liftIO waitEvent
 case e of
   KeyDown _ -> return ()
   MouseButtonDown _ _ _ -> return ()
   _ -> waitForTextAdvance

waitForMenuChoice :: Drawing Int
waitForMenuChoice = do
 e <- liftIO waitEvent
 case e of
   KeyDown (Keysym SDLK_a _ _) -> return 0
   KeyDown (Keysym SDLK_b _ _) -> return 1
   KeyDown (Keysym SDLK_c _ _) -> return 2
   KeyDown (Keysym SDLK_d _ _) -> return 3
   KeyDown (Keysym SDLK_e _ _) -> return 4
   _ -> waitForMenuChoice
