module Main where

import Options.Applicative.Simple hiding ((<>))
import Text.PrettyPrint.Boxes
import System.Process (readProcess)
import Control.Monad.IO.Class
import Data.List.Extra (dropEnd)

import FHue.Hue
import FHue.Types
import FHue.Shell
import FHue.FakeHdfs

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
            simpleOptions "0.0.1"
                          "FHue"
                          "A tool to interact with hdfs through Hue"
                          (pure ()) $
            do addCommand "ls"
                          "List a directory"
                          listAction
                          (strArgument (metavar "path"))
               addCommand "shell"
                          "Open a shell"
                          (const runShell)
                          (pure ())

          let hue = "https://hue-bigplay.bigdata.intraxa/"
              user = "gdubus"
          password <- fmap (dropEnd 1) -- Remove the trailing \n
                           (readProcess "security" ["find-generic-password", "-w", "-s", "PassAXA"] "")
          runHue hue user password hueAction
          --runFakeHdfsT hueAction [mkFile "README.rst", mkDir "group"]

--listAction :: String -> Hue ()
listAction path = do items <- list path
                     liftIO $ printItems items
