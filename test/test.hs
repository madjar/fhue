{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec

import           Network.HTTP.Types.Status (status500)
import           System.Environment        (getEnv)
import           System.FilePath           ((</>))
import           System.IO.Extra           (withTempDir)

import           FHue.Class
import           FHue.Hue
import           FHue.Types
import           System.Keychain

main :: IO ()
main = hspec $ do
  let hue = "https://hue-bigplay.bigdata.intraxa/"
  Just (login, password) <- runIO $ getLogin hue
  let runIt = runHue hue login password

  -- Check that the saved login works
  runIO $ runIt (return ())

  let hdfsHome = "/user" </> login

  describe "all commands" $
    it "make it clear that login failed" $
      runHue hue "badlogin" "badpassword" (return ()) `shouldThrow` (== LoginFailed)

  describe "ls" $ do
    it "can list files in /" $ do
      dirs <- runIt $ list "/"
      map itemName dirs `shouldContain` ["user"]

    it "provides a meaningful error on non-existing dir" $
      runIt (list "/nonexisting/dir") `shouldThrow` (== HueError status500 "Cannot access: /nonexisting/dir. ")

  describe "put" $ do
    it "can upload a file" $ withTempDir $ \tmpDir -> do
      let file = tmpDir </> "testFile"
      writeFile file "hello"
      runIt $ upload file hdfsHome

      files <- runIt $ list hdfsHome
      map itemName files `shouldContain` ["testFile"]

  describe "get" $ do
    it "can download a file" $ withTempDir $ \tmpDir -> do
      runIt $ downloadToDir (hdfsHome </> "testFile") tmpDir
      readFile (tmpDir </> "testFile") `shouldReturn` "hello"

  describe "rm" $ do
    it "can delete a file" $ do
      runIt $ remove (hdfsHome </> "testFile")
      files <- runIt $ list hdfsHome
      map itemName files `shouldNotContain` ["testFile"]

