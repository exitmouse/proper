{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as M

import Visnov
import VisnovDesc

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
                         , ("box", M.singleton "base" "img/box.png")
                         ]
    bgMap :: M.Map BackgroundID FilePath
    bgMap = M.singleton "paris" "img/paris.png"

game :: Visnov Int ()
game = do
  e <- getCharacter "Espen"
  talk e $ do
    "Text" :: Dialogue Int ()
    "Next line" :: Dialogue Int ()
    "Third line"
  getChoice [ ("Agree with him", charAgree e)
            , ("Disagree!!", charAgree e)
            , ("Fly a plane", espenflyplane)
            ]

charAgree :: Character -> Visnov Int ()
charAgree c = do
  talk c $ do
    "I totally agree with you" :: Dialogue Int ()
    "Yep that's a good idea." :: Dialogue Int ()
  end "You died."

espenflyplane :: Visnov Int ()
espenflyplane = do
  end "You died."

loop :: Visnov Int ()
loop = getChoice [ ("Exit", espenflyplane)
                 , ("Repeat", loop)
                 ]

end :: String -> Visnov a ()
end = undefined
