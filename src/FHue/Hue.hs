{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FHue.Hue (Hue, runHue, HueException (..)) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy.Char8              as L
import           Data.List                               (isInfixOf)
import           Data.Text                               (Text)
import           Data.Typeable
import           Network.HTTP.Types.Status               (statusIsSuccessful)
import           Network.Wreq
import           Network.Wreq.Extras                     (partFileWithProgress,
                                                          withSessionOpenSSL)
import qualified Network.Wreq.Session                    as Sess
import           Network.Wreq.Types                      (Postable)
import           System.Console.Haskeline.MonadException

import           FHue.Class

data HueConfig = HueConfig { hueAddress   :: String
                           , hueSession   :: Sess.Session
                           , hueCsrfToken :: B.ByteString}

newtype Hue a = Hue {
    runH :: StateT HueConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadState HueConfig, MonadException)

data HueException = LoginFailed | HueError Status Text | HueFailure Status L.ByteString deriving (Show, Typeable)

instance Exception HueException


instance MonadHdfs Hue where
  list path = do
    r <- hGet ("filebrowser/view=" ++ path ++ "?format=json&pagesize=100") -- TODO use asJSON
    when (hasManyPages r)
      (liftIO $ putStrLn "Warning: we only show the first 1000 items.")
    let items = r ^.. responseBody . key "files" . values . to parse
    return items

    where hasManyPages request = case request ^? responseBody . key "page" . key "num_pages" . _Integral of
                                   Just n -> n > 1
                                   Nothing -> False
          parse o = case fromJSON o of
                      Success x -> x
                      Error e -> error $ show o ++ "\n" ++ e


  upload file destination = do
    -- We need the dest both in GET and POST, because hue use GET to determine
    -- where to put the temporary file
    let url = "/filebrowser/upload/file?dest=" ++ destination
    r <- hPost url [ partFileWithProgress "hdfs_file" file
                   , partString "dest" destination]
    liftIO $ L.putStrLn (r ^. responseBody)


  download source destination = do
    -- TODO loads the full file in memory, but doing otherwise would require a move to http-client
    r <- hGet ("/filebrowser/download=" ++ source)
    liftIO $ L.writeFile destination (r ^. responseBody)
    liftIO $ putStrLn ("Downloaded to " ++ destination)


  remove file =
    void $ hPost "/filebrowser/rmtree" [ partString "path" file]

-- | Run hue with given server address, username and password (in that order)
runHue :: String -> String -> String -> Hue a -> IO a
runHue addr username password hue = withSessionOpenSSL run
  where run sess = evalStateT (runH hueWithLogin) (HueConfig addr sess B.empty)
        hueWithLogin = login username password >> hue


-- | Log the user in on Hue
login :: String -> String -> Hue ()
login username password = do
  -- TODO reuse cookie to save time
  hGet "accounts/login/"  -- To get a first CSRF token
  r <- hPost "accounts/login/" [ "username" := username, "password" := password]
  when ("Error" `isInfixOf` (r ^. responseBody . to L.unpack))  -- TODO ugly
    (throw LoginFailed)


-- | Make a get request on Hue
hGet :: String -> Hue (Response L.ByteString)
hGet url = liftWreq Sess.getWith defaults url


-- | Make a post request on Hue
hPost :: Postable a => String -> a -> Hue (Response L.ByteString)
hPost url payload = do
  csrftoken <- gets hueCsrfToken

  let options = defaults & header "X-CSRFToken" .~ [csrftoken]
                         & header "Referer" .~ ["https://hue-bigplay.bigdata.intraxa/accounts/login/"]
  hPostWith options url payload


-- | Make a post request on Hue with options
hPostWith :: Postable a => Options -> String -> a -> Hue (Response L.ByteString)
hPostWith opts url payload = liftWreq (\o s u -> Sess.postWith o s u payload) opts url



-- | Lift a Wreq action to work on Hue
liftWreq :: (Options -> Sess.Session -> String -> IO (Response L.ByteString)) -> Options -> String -> Hue (Response L.ByteString)
liftWreq action opts url = do addr <- gets hueAddress
                              sess <- gets hueSession
                              let opts' = opts & checkStatus ?~ (\_ _ _ -> Nothing) -- Never throw an exception on http error
                              r <- liftIO $ action opts' sess (addr ++ url)
                              if statusIsSuccessful (r ^. responseStatus) && not (hasJsonError r)
                                then do updateCsrfToken r
                                        return r
                                else throw (errorFor r)
  where errorFor :: Response L.ByteString -> HueException
        errorFor request = case request ^? responseBody . key "message" . _String of
                             Just m -> HueError (request ^. responseStatus) m
                             Nothing -> HueFailure (request ^. responseStatus) (request ^. responseBody)

        hasJsonError :: Response L.ByteString -> Bool
        hasJsonError request = case request ^? responseBody . key "status" . _Integral of
                                 Just 0 -> False
                                 Just _ -> True
                                 Nothing -> False

        updateCsrfToken :: Response a -> Hue ()
        updateCsrfToken response = let token = response ^?! responseCookie "csrftoken" . cookieValue
                                   in modify (\s -> s { hueCsrfToken = token })
