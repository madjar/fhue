{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleInstances,
             MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
module FHue.AilTypes where

import Control.Monad.State.Class (MonadState, state)
import Data.Map.Strict (Map)
import Control.Lens
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict hiding (state)
import System.Console.Haskeline

-- TODO it's clear that this need to die

data FileType = Directory | FileAA | DoesNotExist deriving Show

data AilState = AilState
  { _currentDirectory :: !String
  , _filesCache :: Map FilePath FileType
  , _lsCache :: Map FilePath (Either String String)
  } deriving Show
makeLenses ''AilState

type Ail m = StateT AilState m

instance Monad m => MonadState AilState (InputT (Ail m)) where
  state = lift . state
