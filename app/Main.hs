{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad.IO.Class
import           Options.Applicative.Simple hiding ((<>))
import           Paths_fhue                 as Meta
import           System.Console.Regions     (displayConsoleRegions)
import           System.FilePath        ((</>))

import           FHue.Display
import           FHue.Hue
import           FHue.Class
import           FHue.Types
import           System.Keychain

main :: IO ()
main = do (opts, hueAction) <-
            simpleOptions $(simpleVersion Meta.version)
                          "FHue"
                          "A tool to interact with hdfs through Hue"
                          (pure ()) $
            do addCommand "ls"
                          "List a directory"
                          (\path cur -> listAction (cur </?> path))
                          (optional $ strArgument (metavar "PATH"))
               addCommand "put"
                          "Upload a file"
                          (\(file, dest) cur -> upload file (cur </?> dest))
                          ((,) <$> strArgument (metavar "FILE")
                               <*> optional (strArgument (metavar "DESTINATION")))
               addCommand "get"
                          "Download a file to target directory"
                          (\(file, dest) cur -> downloadToDir (cur </> file) dest)
                          ((,) <$> strArgument (metavar "FILE")
                               <*> strArgument (metavar "DESTINATION"))
               addCommand "rm"
                          "Delete a file or directory"
                          (\file cur -> remove (cur </> file))
                          (strArgument (metavar "FILE"))
               addCommand "edit"
                          "Edit a file with your $EDITOR"
                          (\file cur -> edit (cur </> file))
                          (strArgument (metavar "FILE"))
               -- addCommand "shell"
               --            "Open a shell"
               --            (const runShell)
               --            (pure ())
          runMyHue hueAction
          --runFakeHdfsT hueAction [mkFile "README.rst", mkDir "group"]

runMyHue :: (String -> Hue a) -> IO a
runMyHue hueAction = do
  let hue = "https://hue-bigplay.bigdata.intraxa/"
  (login, password) <- getLogin hue >>= \case
    Just result -> return result
    Nothing -> askAndSetLogin hue
  let userHome = "/user" </> login
  displayConsoleRegions $ runHue hue login password (hueAction userHome)
    --`catch` \case LoginFailed -> askAndSetLogin hue >> runMyHue hueAction


-- | Like '(</>)', but the second argument is an option
(</?>) :: FilePath -> Maybe FilePath -> String
current </?> (Just path) = current </> path
current </?> Nothing = current


listAction :: String -> Hue ()
listAction path = do items <- list path
                     liftIO $ printItems items

