{-# OPTIONS_GHC -Wall #-}

module VisnovDesc where

--import qualified Graphics.DrawingCombinators as DC
import qualified Data.Map as M 

data World = World
  { characterMap :: M.Map UnitID Unit
  , paragraphMap :: M.Map ParaID Paragraph
  , stagingMap :: M.Map StageID Staging
  , leTime :: Time
  }

type UnitID, ParaID, StageID, StatName = String

type Menu = [(String, Event)]

type Paragraph = [Event]

type Staging = (RoomID, [UnitID, Position])

type Position = (Double, Double)

data Event = 
    AdvanceTime Time
  | AffectStat UnitID StatID Int
 -- | AffectAffinity UnitID UnitID Int
  | ExitStage UnitID
  | InvertGlobalBool BoolID
  | Line UnitID String
  | MenuAsk Menu
  | PlaceUnit UnitID Path Position
  | SetGlobalBool BoolID Bool
  | SetGlobalNote NoteID String

data Character = Character
  { defaultFrame :: Path
  , statBlock :: M.Map StatName Int
  }
