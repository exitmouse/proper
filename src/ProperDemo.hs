{-# LANGUAGE OverloadedStrings #-}
module ProperDemo where

import qualified Data.Map.Strict as M
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (modify, get)

import Visnov
import VisnovDesc
import Sound

type Affinity = Int

main = do
  putStrLn "Ok."
  world <- loadWorld
  runVisnov game world 0 

pembertonLikes, pembertonDislikes :: Visnov Affinity ()
pembertonLikes = modify $ \ aff -> aff + 1
pembertonDislikes = modify $ \ aff -> aff - 1

loadWorld :: IO World
loadWorld = setupWorld charMap bgMap
  where
    charMap :: M.Map CharacterID (M.Map PoseID FilePath)
    charMap = M.fromList [ ("duderton", M.singleton "blank_stare" "img/duderton.png")
                         , ("archibald", M.singleton "blank_stare" "img/archibald.png")
                         , ("pemberton", M.fromList [("blank_stare", "img/pemberton.png"), ("b", "img/pemberton_b.png")])
                         , ("box", M.singleton "base" "img/box.png")
                         ]
    bgMap :: M.Map BackgroundID FilePath
    bgMap = M.fromList [ ("paris", "img/paris.png")
                       , ("space_wedge", "img/space_wedge.png")
                       ]

onStage = pose "blank_stare"

game = do
  liftIO $ playBackground "suave.wav"
  setBackground "paris"
  p <- getCharacter "pemberton"
  as p $ do
    onStage
    say "The Proper Visual Novel EngineLibraryFramework makes it easy to write"
    say "your own interactive novel experience!"
  getChoice [ ("That sounds awesome!", awesome)
            , ("It's called an ELF?", elf)
            ]

awesome = do
  a <- getCharacter "archibald"
  p <- getCharacter "pemberton"
  as a $ do
    onStage
    say "I am skeptical of the computing machine's ability to capture a novel."
  as p $ do
    say "Prepare to be amazed."
  getChoice [ ("I too am skeptical.", pembertonDislikes >> skeptic)
            , ("I am very prepared.", pembertonLikes >> ready)
            ]

elf = do
  p <- getCharacter "pemberton"
  as p $ do
    onStage
    say "Yup!"
  getChoice [("Cool!", awesome)]

skeptic = do
  a <- getCharacter "archibald"
  p <- getCharacter "pemberton"
  as a $ do
    onStage
    say "Ah!  A man with reason superior to this ape, Pemberton's!"
  as p $ do
    say "Trust me just a little bit, my fair collective of judges."
  getChoice [ ("...", ready)]

ready = do
  a <- getCharacter "archibald"
  p <- getCharacter "pemberton"
  affinity <- get 
  as p $ do
    pose "b"
    say "AWWWW YEAH!  I HAVE TRANSFORMED INTO A HERSHEY KISS ANGEL!"
    say "TOP THAT, BITCH!"
  as a $ do
    pose "blank_stare"
    say "I am defeated."
  if affinity > 0
    then
      as p $ do
        say "I'm glad you believed in me, judges."
    else
      as p $ do
        say "Don't you wish you trusted me?"
