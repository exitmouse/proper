{-# OPTIONS_GHC -Wall #-}

module VisnovDesc where

--import qualified Graphics.DrawingCombinators as DC
import qualified Data.Map as M 
import qualified Data.Time as T

data World = World
  { worldCharacterMap :: M.Map PersonID Person
  , worldBackgroundMap :: M.Map BackgroundID Background
  , worldParagraphMap :: M.Map ParaID Paragraph
  , worldTime :: T.TimeOfDay
  }

type PersonID = String
type PoseID = String
type ParaID = String
type StatName = String
type BoolID = String
type NoteID = String

type Menu = [(String, ParaID)]

type Paragraph = [Event]

type Position = (Double, Double)

data Event = AdvanceTime T.TimeOfDay
           | AffectStat PersonID StatName Int
           | ExitStage PersonID
           | InvertGlobalBool BoolID
           | Line PersonID String
           | MenuAsk Menu
           | PlaceUnit PersonID FilePath Position
           | SetGlobalBool BoolID Bool
           | SetGlobalNote NoteID String

data Person = Person
  { personFrames :: M.Map PoseID FilePath
  , personStatBlock :: M.Map StatName Int
  }

type Background = FilePath
