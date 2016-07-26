{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad.IO.Class
import           Options.Applicative.Simple hiding ((<>))
import           Paths_fhue                 as Meta
import           System.Console.Regions     (displayConsoleRegions)
import           Text.PrettyPrint.Boxes

import           System.Keychain

import           FHue.Hue
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

listAction :: String -> Hue ()
listAction path = do items <- list path
                     liftIO $ printItems items

