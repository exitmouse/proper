{-# OPTIONS_GHC -Wall #-}

module VisnovDesc where

import Data.Functor ((<$>))
import qualified Data.Map.Strict as M 
import Data.Traversable (traverse)
import Sprite

type BackgroundID = String
type CharacterID = String
type PoseID = String

type Background = Sprite
type Pose = Sprite

data World = World
  { worldCharacterMap :: M.Map CharacterID Character
  , worldBackgroundMap :: M.Map BackgroundID Background
  }

data Character = Character
  { characterFrames :: M.Map PoseID Pose
  }

setupCharacter :: (M.Map PoseID FilePath) -> IO Character
setupCharacter fPoseMap = Character <$> traverse loadSprite fPoseMap

setupWorld :: (M.Map CharacterID (M.Map PoseID FilePath)) -> (M.Map BackgroundID FilePath) -> IO World
setupWorld fCharMap fBgMap = do
  charMap <- traverse setupCharacter fCharMap
  bgMap <- traverse loadSprite fBgMap
  return $ World charMap bgMap
