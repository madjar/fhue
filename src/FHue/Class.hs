module FHue.Class (MonadHdfs (..), downloadToDir, edit) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe             (fromMaybe)
import           System.Environment     (lookupEnv)
import           System.FilePath        (takeDirectory, takeFileName, (</>))
import           System.IO.Extra        (newTempDir)
import           System.Process         (callProcess)

import           FHue.Types

type HuePath = String

class Monad m => MonadHdfs m where
  -- | List the content of a hue directory
  list :: HuePath -> m [Item]

  -- | Upload a local file to a hue directory.
  -- The newly created file will have the same name as the local file
  upload :: FilePath -> HuePath -> m ()

  -- | Download a hue file to a local file
  -- To download to a directory and keep the same filename, see 'downloadToDir'
  download :: HuePath -> FilePath -> m ()

  -- | Delete a file on hue
  remove :: HuePath -> m ()


-- | Download a hue file to a local directory, keeping its name
downloadToDir :: MonadHdfs m => HuePath -> FilePath -> m ()
downloadToDir source destDir = do
  let fileName = takeFileName source
      output = destDir </> fileName
  download source output


-- | Download a file to edit it with $EDITOR, then upload it back
edit :: (MonadIO m, MonadHdfs m) => String -> m ()
edit file = do
  (tmpDir, deleteTmpFile) <- liftIO  newTempDir
  let fileName = takeFileName file
      tmpFile = tmpDir </> fileName
  download file tmpFile
  editor <- liftIO $ fromMaybe "nano" <$> lookupEnv "EDITOR"
  liftIO $ callProcess editor [tmpFile]
  remove file
  upload tmpFile (takeDirectory file)
  liftIO  deleteTmpFile
