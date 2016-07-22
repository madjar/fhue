{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FHue.Hue (Hue, runHue, upload, downloadToDir, HueException (..)) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8              as L
import           Network.Wreq
import           Network.Wreq.Extras                     (withSessionOpenSSL, partFileWithProgress)
import qualified Network.Wreq.Session                    as Sess
import           Network.Wreq.Types                      (Postable)
import           System.Console.Haskeline.MonadException
import Data.List (isInfixOf)
import Data.Typeable
import Control.Exception
import Network.HTTP.Types.Status (statusIsSuccessful)
import Data.Text (Text)
import System.FilePath (takeFileName, (</>))

import           FHue.Types

data HueConfig = HueConfig { hueAddress :: String
                           , hueSession :: Sess.Session}

newtype Hue a = Hue {
    runH :: ReaderT HueConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HueConfig, MonadException)

data HueException = LoginFailed | HueError Status Text | HueFailure Status L.ByteString deriving (Show, Typeable)

instance Exception HueException


-- | Lift a Wreq action to work on Hue
liftWreq :: (Options -> Sess.Session -> String -> IO (Response L.ByteString)) -> Options -> String -> Hue (Response L.ByteString)
liftWreq action opts url = do addr <- asks hueAddress
                              sess <- asks hueSession
                              let opts' = opts & checkStatus ?~ (\_ _ _ -> Nothing)
                              r <- liftIO $ action opts' sess (addr ++ url)
                              if statusIsSuccessful (r ^. responseStatus) && not (hasJsonError r)
                                then return r
                                else throw (errorFor r)
  where errorFor request = case request ^? responseBody . key "message" . _String of
                             Just m -> HueError (request ^. responseStatus) m
                             Nothing -> HueFailure (request ^. responseStatus) (request ^. responseBody)
        hasJsonError request = case request ^? responseBody . key "status" . _Integral of
                                 Just 0 -> False
                                 Just _ -> True
                                 Nothing -> False



hPostWith :: Postable a => Options -> String -> a -> Hue (Response L.ByteString)
hPostWith opts url payload = liftWreq (\o s u -> Sess.postWith o s u payload) opts url


-- | Make a get request on Hue
hGet :: String -> Hue (Response L.ByteString)
hGet = liftWreq Sess.getWith defaults


-- | Make a post request on Hue
hPost :: Postable a => String -> a -> Hue (Response L.ByteString)
hPost url payload = do
  -- TODO keep csrf and referer between actions
  r <- hGet "accounts/login/"
  _ <- hGet "filebrowser/view=/user/gdubus"

  let csrftoken = r ^?! responseCookie "csrftoken" . cookieValue
      options = defaults & header "X-CSRFToken" .~ [csrftoken]
                         & header "Referer" .~ ["https://hue-bigplay.bigdata.intraxa/accounts/login/"]
  hPostWith options url payload


-- | Log the user in on Hue
login :: String -> String -> Hue ()
login username password = do
  r <- hGet "accounts/login/"
  -- TODO reuse cookie to save time
  r <- hPost "accounts/login/" [ "username" := username, "password" := password]
  when ("Error" `isInfixOf` (r ^. responseBody . to L.unpack))  -- TODO ugly
    (throw LoginFailed)

upload :: FilePath -> String -> Hue ()
upload file destination = do
  -- We need the dest both in GET and POST, because hue use GET to determine
  -- where to put the temporary file
  let url = "/filebrowser/upload/file?dest=" ++ destination
  r <- hPost url [ partFileWithProgress "hdfs_file" file
                 , partString "dest" destination]
  liftIO $ L.putStrLn (r ^. responseBody)

downloadToFile :: String -> FilePath -> Hue ()
downloadToFile source destination = do
  -- TODO loads the full file in memory, but doing otherwise would require a move to http-client
  r <- hGet ("/filebrowser/download=" ++ source)
  liftIO $ L.writeFile destination (r ^. responseBody)
  liftIO $ putStrLn ("Downloaded to " ++ destination)

downloadToDir :: String -> FilePath -> Hue ()
downloadToDir source destDir = do
  let fileName = takeFileName source
      output = destDir </> fileName
  downloadToFile source output

-- | Run hue with given server address, username and password (in that order)
runHue :: String -> String -> String -> Hue a -> IO a
runHue addr username password hue = withSessionOpenSSL run
  where run sess = runReaderT (runH hueWithLogin) (HueConfig addr sess)
        hueWithLogin = login username password >> hue

parse o = case fromJSON o of
            Success x -> x
            Error e -> error $ show o ++ "\n" ++ e

instance MonadHdfs Hue where
  list path = do r <- hGet ("filebrowser/view=" ++ path ++ "?format=json") -- TODO use asJSON
                 let items = r ^.. responseBody . key "files" . values . to parse
                 return items
