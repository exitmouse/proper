{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Visnov ( Visnov
              , Dialogue
              , runVisnov
              , getCharacter
              , getChoice
              , pose
              , talk
              , background
              ) where

import Control.Monad (Monad, (>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader, Reader(..), runReader, ReaderT(..), runReaderT, ask, asks, local)
import Control.Monad.State (StateT, runStateT, get, put)
import qualified Data.Map as M
import Data.String (IsString, fromString)
import VisnovDesc (World(..), Character(..))

type Visnov s = ReaderT World (StateT s IO)
type Event s = Visnov s ()
newtype Dialogue s a = Dialogue { unDialogue :: (ReaderT Character (Visnov s) a)} deriving (Monad)
instance IsString (Dialogue s ()) where
  fromString s = Dialogue $ liftIO (writeGameText s)

runVisnov :: Visnov s a -> World -> s -> IO (a, s)
runVisnov v w s = runStateT (runReaderT v w) s

talk :: Character -> Dialogue s a -> Visnov s a
talk = runDialogueWith

runDialogueWith :: Character -> Dialogue s a -> Visnov s a
runDialogueWith p d = runReaderT (unDialogue d) p

getCharacter :: (MonadReader World m)
          => String
          -> m (Character)
getCharacter s = do
  m_character <- asks $ (M.lookup s) . worldCharacterMap
  case m_character of
    Nothing -> error "Requested character does not exist"
    Just character -> return character

-- Requires IO, so isn't more general
getChoice :: [(String, Visnov s a)] -> Visnov s a
getChoice [] = undefined -- Ha Ha
getChoice (x:xs) = snd x -- Choose the first one TODO

pose :: String -> Dialogue u ()
pose s = Dialogue $ do
  character <- ask
  let m_frame = M.lookup s (characterFrames character)
  case m_frame of
    Nothing -> error "No such pose"
    Just frame -> ReaderT $ \r -> (drawChar frame :: Event u)

drawChar :: FilePath -> Event u
drawChar f = undefined -- TODO

background :: FilePath -> Event u
background f = undefined -- TODO

writeGameText :: String -> IO ()
writeGameText = undefined
