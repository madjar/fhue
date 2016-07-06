{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FHue.Hue (Hue, runHue) where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy                    as L
import           Network.Wreq
import           Network.Wreq.Extras                     (withSessionOpenSSL)
import qualified Network.Wreq.Session                    as Sess
import           Network.Wreq.Types                      (Postable)
import           System.Console.Haskeline.MonadException

import           FHue.Types

data HueConfig = HueConfig { hueAddress :: String
                           , hueSession :: Sess.Session}

newtype Hue a = Hue {
    runH :: ReaderT HueConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader HueConfig, MonadException)


-- | Lift a Wreq action to work on Hue
liftWreq :: (Options -> Sess.Session -> String -> IO a) -> Options -> String -> Hue a
liftWreq action opts url = do addr <- asks hueAddress
                              sess <- asks hueSession
                              --let opts' = opts & proxy ?~ httpProxy "localhost" 8080
                              liftIO $ action opts sess (addr ++ url)


hPostWith :: Postable a => Options -> String -> a -> Hue (Response L.ByteString)
hPostWith opts url payload = liftWreq (\o s u -> Sess.postWith o s u payload) opts url


-- | Make a get request on Hue
hGet :: String -> Hue (Response L.ByteString)
hGet = liftWreq Sess.getWith defaults


-- | Make a post request on Hue
hPost :: Postable a => String -> a -> Hue (Response L.ByteString)
hPost = hPostWith defaults



-- | Log the user in on Hue
login :: String -> String -> Hue ()
login username password = do
  r <- hGet "accounts/login/"
  -- TODO reuse cookie to save time
  let csrftoken = r ^?! responseCookie "csrftoken" . cookieValue
      options = defaults & header "Referer" .~ ["https://hue-bigplay.bigdata.intraxa/accounts/login/"]
  void $ hPostWith options "accounts/login/" [ "username" := username
                                             , "password" := password
                                             , "csrfmiddlewaretoken" := csrftoken]


-- | Run hue with given server address, username and password (in that order)
runHue :: String -> String -> String -> Hue a -> IO a
runHue addr username password hue = withSessionOpenSSL run
  where run sess = runReaderT (runH hueWithLogin) (HueConfig addr sess)
        hueWithLogin = login username password >> hue

parse o = case fromJSON o of
            Success x -> x
            Error e -> error $ show o ++ "\n" ++ e

instance MonadHdfs Hue where
  list path = do r <- hGet ("filebrowser/view=" ++ path ++ "?format=json")
                 let items = r ^.. responseBody . key "files" . values . to parse
                 return items
