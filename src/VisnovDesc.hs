{-# OPTIONS_GHC -Wall #-}

module VisnovDesc where

--import qualified Graphics.DrawingCombinators as DC
import qualified Data.Map as M 
import qualified Data.Time as T

data World = World
  { worldCharacterMap :: M.Map CharacterID Character
  , worldBackgroundMap :: M.Map BackgroundID Background
  , worldParagraphMap :: M.Map ParaID Paragraph
  , worldTime :: T.TimeOfDay
  }

type BackgroundID = String
type CharacterID = String
type PoseID = String
type ParaID = String
type StatName = String
type BoolID = String
type NoteID = String

type Menu = [(String, ParaID)]

type Paragraph = [Event]

type Position = (Double, Double)

data Event = AdvanceTime T.TimeOfDay
           | AffectStat CharacterID StatName Int
           | ExitStage CharacterID
           | InvertGlobalBool BoolID
           | Line CharacterID String
           | MenuAsk Menu
           | PlaceUnit CharacterID FilePath Position
           | SetGlobalBool BoolID Bool
           | SetGlobalNote NoteID String

data Character = Character
  { characterFrames :: M.Map PoseID FilePath
  , characterStatBlock :: M.Map StatName Int
  }

type Background = FilePath
