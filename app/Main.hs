{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List.Extra            (dropEnd)
import           Options.Applicative.Simple hiding ((<>))
import           Paths_fhue                 as Meta
import           System.Console.Haskeline
import           System.Console.Regions     (displayConsoleRegions)
import           System.Exit                (ExitCode (..))
import           System.Process             (readProcess,
                                             readProcessWithExitCode)
import           Text.PrettyPrint.Boxes
import           Text.Regex.TDFA

import           FHue.FakeHdfs
import           FHue.Hue
import           FHue.Shell
import           FHue.Types

printItems :: [Item] -> IO ()
printItems is = printBox . hsep 2  center1 $ allCols
  where colFuncs = [ itemRwx
                   , itemUser
                   , itemGroup
                   , show . itemSize
                   , itemMtime
                   , itemName ]
        makeCol f = vcat right $ map (text . f) is
        allCols =  map makeCol colFuncs

main :: IO ()
main = do (opts, hueAction) <-
            simpleOptions $(simpleVersion Meta.version)
                          "FHue"
                          "A tool to interact with hdfs through Hue"
                          (pure ()) $
            do addCommand "ls"
                          "List a directory"
                          listAction
                          (strArgument (metavar "PATH"))
               addCommand "put"
                          "Upload a file"
                          (uncurry upload)
                          ((,) <$> strArgument (metavar "FILE")
                               <*> strArgument (metavar "DESTINATION"))
               addCommand "get"
                          "Download a file to target directory"
                          (uncurry downloadToDir)
                          ((,) <$> strArgument (metavar "FILE")
                               <*> strArgument (metavar "DESTINATION"))
               addCommand "rm"
                          "Delete a file or directory"
                          remove
                          (strArgument (metavar "FILE"))
               addCommand "edit"
                          "Edit a file with your $EDITOR"
                          edit
                          (strArgument (metavar "FILE"))
               -- addCommand "shell"
               --            "Open a shell"
               --            (const runShell)
               --            (pure ())
          runMyHue hueAction
          --runFakeHdfsT hueAction [mkFile "README.rst", mkDir "group"]

runMyHue hueAction = do
  let hue = "https://hue-bigplay.bigdata.intraxa/"
  (login, password) <- getLogin hue >>= \case
    Just result -> return result
    Nothing -> askAndSetLogin hue
  displayConsoleRegions $ runHue hue login password hueAction
    --`catch` \case LoginFailed -> askAndSetLogin hue >> runMyHue hueAction

getLogin :: String -> IO (Maybe (String, String))
getLogin root = do
  (exitCode, out, err) <- readProcessWithExitCode "security" ["find-generic-password", "-g", "-s", root] ""
  let loginRegex = "\"acct\"<blob>=\"(.*)\""
      passwordRegex = "password: \"(.*)\""
      -- TODO this is very ugly
      [[_, login]] = out =~ loginRegex
      [[_, password]] = err =~ passwordRegex
  if exitCode == ExitSuccess
    then return (Just (login, password))
    else return Nothing

setLogin :: String -> String -> String -> IO ()
setLogin root login password = do
  void $ readProcessWithExitCode "security" ["add-generic-password", "-s", root] ""
  void $ readProcess "security" ["add-generic-password", "-s", root, "-a", login, "-w", password] ""

askAndSetLogin :: String -> IO (String, String)
askAndSetLogin hue = runInputT defaultSettings $ do
  Just login <- getInputLine "login: "
  Just password <- getPassword (Just '*') "pass: "
  liftIO (setLogin hue login password)
  return (login, password)

listAction :: String -> Hue ()
listAction path = do items <- list path
                     liftIO $ printItems items

