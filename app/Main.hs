{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Exception.Extra    (catchBool)
import           Control.Monad.IO.Class
import           Options.Applicative.Simple
import           Paths_fhue                 as Meta
import           System.Console.Regions     (displayConsoleRegions)
import           System.Environment         (lookupEnv)
import           System.FilePath            ((</>))

import           FHue.Class
import           FHue.Display
import           FHue.Hue
import           FHue.Types
import           System.Keychain

main :: IO ()
main = do envUrl <- lookupEnv "FHUE_URL"
          (fhueUrl, hueAction) <-
            simpleOptions $(simpleVersion Meta.version)
                          "FHue"
                          "A tool to interact with hdfs through Hue"
                          (strOption (long "url"
                                   <> short 'u'
                                   <> metavar "HUE_URL"
                                   <> help "Url of the Hue server to talk to. Defaults to the FHUE_URL environment variable"
                                   <> case envUrl of
                                        Just url -> value url <> showDefault
                                        Nothing -> mempty)
                          ) $
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
          runMyHue fhueUrl hueAction
          --runFakeHdfsT hueAction [mkFile "README.rst", mkDir "group"]

runMyHue :: String -> (String -> Hue a) -> IO a
runMyHue fhueUrl hueAction = displayConsoleRegions $ do
  (login, password) <- getLogin fhueUrl >>= \case
    Just result -> return result
    Nothing -> askAndSetLogin fhueUrl
  let userHome = "/user" </> login
  catchBool (== LoginFailed)
            (runHue fhueUrl login password (hueAction userHome))
            (\_ -> askAndSetLogin fhueUrl >> runMyHue fhueUrl hueAction)


-- | Like '(</>)', but the second argument is an option
(</?>) :: FilePath -> Maybe FilePath -> String
current </?> (Just path) = current </> path
current </?> Nothing = current


listAction :: String -> Hue ()
listAction path = do items <- list path
                     liftIO $ printItems items

