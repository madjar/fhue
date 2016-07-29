module System.Keychain (getLogin, setLogin, askAndSetLogin) where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.Console.Haskeline
import           System.Exit                (ExitCode (..))
import           System.Process             (readProcess,
                                             readProcessWithExitCode)
import           Text.Regex.TDFA


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
  void $ readProcessWithExitCode "security" ["delete-generic-password", "-s", root] ""
  void $ readProcess "security" ["add-generic-password", "-s", root, "-a", login, "-w", password] ""

askAndSetLogin :: String -> IO (String, String)
askAndSetLogin hue = runInputT defaultSettings $ do
  Just login <- getInputLine "login: "
  Just password <- getPassword (Just '*') "pass: "
  liftIO (setLogin hue login password)
  return (login, password)
