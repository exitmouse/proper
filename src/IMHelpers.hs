module IMHelpers ( Drawing
                 , Env(..)
                 , State(..)
                 , drawGameText
                 , drawGameTextBox
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
  drawGameTextBox
  drawGameText (cname ++ ": " ++ s)

writeMenuText :: [String] -> Drawing ()
writeMenuText s = do
  drawMenuTextBox
  drawMenuText s

-- Blit functions don't flip. Draw functions do.
drawGameTextBox :: Drawing ()
drawGameTextBox = do
  tgt <- asks surface
  let x1 = 35
      y1 = 480
      x2 = 605
      y2 = 295
  _ <- liftIO $ box tgt (Rect x1 y1 x2 y2) (Pixel 0xffbe6cbb)
  return ()

drawMenuTextBox :: Drawing ()
drawMenuTextBox = do
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
  void $ liftIO $ blitSurface im Nothing tgt $ Just $ Rect x y w h

drawText :: Int -> Int -> String -> Drawing ()
drawText x y s = do
  blitText x y s
  tgt <- asks surface
  liftIO $ flip tgt
  return ()

drawMenuText :: [String] -> Drawing ()
drawMenuText s = do
  let l = length s
      min = 40
      max = 440
      offset = (max-min) `div` l
  drawAll s min offset

drawAll :: [String] -> Int -> Int -> Drawing ()
drawAll [] _ _= return ()
drawAll [s] min _ = drawText 40 min s
drawAll (s:xs) min offset = blitText 40 min s >> drawAll xs (min+offset) offset

drawGameText :: String -> Drawing ()
drawGameText = drawText 40 300

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
