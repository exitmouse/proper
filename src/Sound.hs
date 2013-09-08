module Sound where

{- Requires (Haskell) sdl, sdl-mixer, (non-Haskell) sdl_mixer -}

import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.Async
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

audioConsts :: IO ()
audioConsts = openAudio audioRate audioFormat audioChannels audioBuffers
  where audioRate     = 22050
        audioFormat   = Mix.AudioS16LSB
        audioChannels = 2
        audioBuffers  = 4096
        anyChannel    = -1

-- | single sound effect
play :: String -> IO (Async ())
play s = async $ do
  SDL.init [SDL.InitAudio]
  result <- audioConsts
  sound <- Mix.loadWAV s
  ch1 <- Mix.playChannel (-1) sound 0
  fix $ \loop -> do
    SDL.delay 50
    stillPlaying <- numChannelsPlaying
    when (stillPlaying /= 0) loop
  Mix.closeAudio
  SDL.quit

-- | looping background music
playBackground :: String -> IO (Async ())
playBackground s = async $ do
  SDL.init [SDL.InitAudio]
  result <- audioConsts
  sound <- Mix.loadWAV s
  ch1 <- Mix.playChannel 2 sound (-1)
  resume 2
  fix $ \loop -> do
    SDL.delay 50
    stillPlaying <- numChannelsPlaying
    when (stillPlaying /= 0) loop
  Mix.closeAudio
  SDL.quit

pauseBackgroundMusic :: IO ()
pauseBackgroundMusic = pause 2

resumeBackgroundMusic :: IO ()
resumeBackgroundMusic = resume 2

endBackgroundMusic :: IO ()
endBackgroundMusic = haltChannel 2

example :: IO ()
example = do
  playBackground "../sounds/click.wav"
  SDL.delay 1000
  play "../sounds/song.wav"
  SDL.delay 1000
  pauseBackgroundMusic
  putStrLn "Hi!"
