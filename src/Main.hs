{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as M
import Control.Monad.IO.Class (liftIO)

import Visnov
import VisnovDesc
import Sound

main = do
  putStrLn "Ok."
  world <- loadWorld
  runVisnov game world 0

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

game :: Visnov Int ()
game = do
  liftIO $ playBackground "song.wav"
  setBackground "paris"
  a <- getCharacter "archibald"
  p <- getCharacter "pemberton"
  as a $ do
    pose "blank_stare"
    say "Now I will terrorize Paris!!"
    say "No one can stop me!!"
    say "Not even... Pemberton!!"
  as p $ do
    pose "blank_stare"
    say "--Not so fast!!"
    say "Yes, it is me, Pemberton."
    say "Transform!!"
    pose "b"
    say "Hiyaaa!"
    say "How do you like my new form?"
  getChoice [ ("Yes, it's quite nice", pemberNice)
            , ("You are evil too!!", pemberEvil)
            , ("Fly a plane", flyplane)
            ]

pemberEvil :: Visnov Int ()
pemberEvil = do
  a <- getCharacter "archibald"
  p <- getCharacter "pemberton"
  as p $ do
    say "Now I will terrorize the space wedge!!"
    background "space_wedge"
    say "No one can stop me!!"
    say "Not even... Archibald!!"
  as a $ do
    pose "blank_stare"
    say "--Not so fast!!"
    say "Yes, it is me, Archibald."
    say "Wait we've done this before."
  end "You were too silly and you died. You were in space, even!! You can't breathe in space, that's ridiculous. Stop it."

pemberNice :: Visnov Int ()
pemberNice = do
  a <- getCharacter "archibald"
  p <- getCharacter "pemberton"
  as p $ do
    say "My marshmallowy form belies my true ability!"
  as a $ do
    pose "blank_stare"
    say "--Not so fast!!"
    say "You are way too fast!!"
    say "Seriously slow down!!"
  end "You were too fast and you died."

charAgree :: Character -> Visnov Int ()
charAgree c = do
  as c $ do
    say "I totally agree with you"
    say "Yep that's a good idea."
  end "You died."

flyplane :: Visnov Int ()
flyplane = do
  end "You flew a plane into a wall and died."

loop :: Visnov Int ()
loop = getChoice [ ("Exit", flyplane)
                 , ("Repeat", loop)
                 ]

end :: String -> Visnov a ()
end = liftIO . putStrLn
