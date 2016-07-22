{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
module FHue.Shell where

import           Control.Lens                     (use, uses, (.=))
import           Control.Monad.Trans
import           Control.Monad.Trans.State.Strict hiding (state)
import           Data.List.Extra                  (trim)
import           Data.List.Split
import qualified Data.Map.Strict                  as M
import           System.Console.Haskeline
import           System.FilePath

import           FHue.AilTypes
import           FHue.Completion
import           FHue.Types                       (MonadHdfs, list)

runShell :: (MonadIO m, MonadException m, MonadHdfs m) => m ()
runShell = do let initialState = AilState "/" mempty mempty
                  settings = setComplete ailCompletion defaultSettings
              evalStateT (runInputT settings loop) initialState


loop :: (MonadIO m, MonadException m, MonadHdfs m) => InputT (Ail m) ()
loop = do current <- use currentDirectory
          minput <- getInputLine (current ++ "% ")
          case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do command (splitOn " " $ trim input)
                             loop

command :: (MonadIO m, MonadHdfs m) => [String] -> InputT (Ail m) ()
command ["cd", arg] = do current <- use currentDirectory
                         cd (current </> arg)
command ["ls"] = do current <- use currentDirectory
                    listCmd current
command ["ls", arg] = do current <- use currentDirectory
                         listCmd (current </> arg)
command [] = return ()
command _ = outputStrLn "Unknown command"

cd :: (MonadIO m, MonadHdfs m) => FilePath -> InputT (Ail m) ()
cd path = do cachedStatus <-  uses filesCache (M.lookup path)
             case cachedStatus of
               Just Directory -> currentDirectory .= path
               Just FileAA -> outputStrLn $ "cd: '" ++ path ++ "' is not a directory"
               Just DoesNotExist -> outputStrLn $ "cd: The directory'" ++ path ++ "' does not exist"
               Nothing -> do _ <- lift . lift $ list path --Update the cache
                             cd path

listCmd :: (MonadIO m, MonadHdfs m) => FilePath -> InputT (Ail m) ()
listCmd path = do result <- lift . lift $ list path
                  outputStr $ show result

