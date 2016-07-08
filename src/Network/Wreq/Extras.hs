{-# LANGUAGE OverloadedStrings #-}
module Network.Wreq.Extras
    ( withSessionOpenSSL
    , partFileWithProgress
    ) where

import           Network.Wreq.Session    (Session, withSessionControl)
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session (context)
import Network.HTTP.Client (createCookieJar)
import Network.HTTP.Client.MultipartFormData (Part, partFileRequestBodyM)
import           Data.Conduit                 (ConduitM, await, yield)
import System.Console.AsciiProgress
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import System.IO (hFileSize, withBinaryFile, IOMode (ReadMode))
import Network.HTTP.Conduit (requestBodySource)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Conduit ((=$=))
import Data.Conduit.Binary (sourceFile)
import Control.Monad.Trans.Resource (runResourceT)

withSessionOpenSSL :: (Session -> IO a) -> IO a
withSessionOpenSSL = withOpenSSL . withSessionControl emptyCookieJar managerSettings
  where emptyCookieJar = Just (createCookieJar [])
        managerSettings = opensslManagerSettings context


partFileWithProgress :: Text -> FilePath -> Part
partFileWithProgress name file = partFileRequestBodyM name file $ do
  size <- withBinaryFile file ReadMode hFileSize
  pg <- liftIO $ newProgressBar def { pgTotal = size }
  let source = sourceFile file =$= updateProgress pg
  return $ requestBodySource (fromIntegral size) source


-- From https://github.com/yamadapc/haskell-ascii-progress/blob/master/bin/DownloadExample.hs
updateProgress :: MonadIO m => ProgressBar -> ConduitM ByteString ByteString m ()
updateProgress pg = await >>= maybe (return ()) (\chunk -> do
    let len = ByteString.length chunk
    liftIO $ tickN pg len
    yield chunk
    updateProgress pg)
