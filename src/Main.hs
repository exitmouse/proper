{-# LANGUAGE OverloadedStrings #-}
module Main where

import Visnov
import VisnovDesc (World(..), Character(..))

main = do
  putStrLn "Ok."
  runVisnov game worldData 0

worldData :: World
worldData = undefined

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
