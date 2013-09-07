module Sound where

{- Requires (Haskell) sdl, sdl-mixer, (non-Haskell) sdl_mixer -}

import Control.Monad
import Control.Monad.Fix
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

play :: String -> IO ()
play s = do
  SDL.init [SDL.InitAudio]
  result <- openAudio audioRate audioFormat audioChannels audioBuffers
  classicJungle <- Mix.loadWAV s
  ch1 <- Mix.playChannel anyChannel classicJungle 0
  fix $ \loop -> do
    SDL.delay 50
    stillPlaying <- numChannelsPlaying
    when (stillPlaying /= 0) loop
  Mix.closeAudio
  SDL.quit
  where audioRate     = 22050
        audioFormat   = Mix.AudioS16LSB
        audioChannels = 2
        audioBuffers  = 4096
        anyChannel    = -1

main = play "../LD.wav"
