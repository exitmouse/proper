{-# OPTIONS_GHC -Wall #-}

module VisnovDesc where

--import qualified Graphics.DrawingCombinators as DC
import qualified Data.Map as M 
import qualified Data.Time as T

data World = World
  { characterMap :: M.Map PersonID Person
  , paragraphMap :: M.Map ParaID Paragraph
  , worldTime :: T.TimeOfDay
  }

type PersonID = String
type ParaID = String
type StatName = String
type BoolID = String
type NoteID = String

type Menu = [(String, ParaID)]

type Paragraph = [Event]

type Position = (Double, Double)

data Event = 
    AdvanceTime T.TimeOfDay
  | AffectStat PersonID StatName Int
  | ExitStage PersonID
  | InvertGlobalBool BoolID
  | Line PersonID String
  | MenuAsk Menu
  | PlaceUnit PersonID FilePath Position
  | SetGlobalBool BoolID Bool
  | SetGlobalNote NoteID String

data Person = Person
  { defaultFrame :: FilePath
  , statBlock :: M.Map StatName Int
  }
