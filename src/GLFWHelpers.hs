module GLFWHelpers ( Drawing
                   , Env(..)
                   , State(..)
                   , drawGameText
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

--------------------------------------------------------------------------------

data Env = Env
    { surface :: Surface
    }

data State = State
    { stateWindowWidth     :: !Int
    , stateWindowHeight    :: !Int
    , advanceText          :: !Bool
    }

--------------------------------------------------------------------------------

type Drawing = RWST Env () State IO

drawGameText :: String -> Drawing ()
drawGameText s = do
  tgt <- asks surface
  state <- get
  font <- liftIO $ openFont "/usr/share/fonts/TTF/LiberationMono-Regular.ttf" 12
  im <- liftIO $ renderUTF8Solid font s (Color 1 0 0)
  let w = surfaceGetWidth im
      h = surfaceGetHeight im
      x = 40
      y = 300
  _ <- liftIO $ blitSurface im Nothing tgt $ Just $ Rect x y w h
  liftIO $ flip tgt
  liftIO $ delay 2000
  return ()

--draw :: Drawing ()
--draw = do
--    --env   <- ask
--    --state <- get
--    liftIO $ do
--        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
--
--drawGameText :: String -> Drawing ()
--drawGameText s = do
--  env <- ask
--  liftIO $ do
--    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
--    renderText s
--  processEvents
--  state <- get
--  unless (advanceText state) $ do
--    modify $ \s -> s { advanceText = False }
--    drawGameText s
--
--renderText :: String -> IO ()
--renderText s = return undefined

--getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
--getCursorKeyDirections win = do
--    x0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Up
--    x1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Down
--    y0 <- isPress `fmap` GLFW.getKey win GLFW.Key'Left
--    y1 <- isPress `fmap` GLFW.getKey win GLFW.Key'Right
--    let x0n = if x0 then (-1) else 0
--        x1n = if x1 then   1  else 0
--        y0n = if y0 then (-1) else 0
--        y1n = if y1 then   1  else 0
--    return (x0n + x1n, y0n + y1n)

--getJoystickDirections :: GLFW.Joystick -> IO (Double, Double)
--getJoystickDirections js = do
--    maxes <- GLFW.getJoystickAxes js
--    return $ case maxes of
--      (Just (x:y:_)) -> (-y, x)
--      _              -> ( 0, 0)

--isPress :: GLFW.KeyState -> Bool
--isPress GLFW.KeyState'Pressed   = True
--isPress GLFW.KeyState'Repeating = True
--isPress _                       = False

--------------------------------------------------------------------------------

