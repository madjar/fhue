{-# LANGUAGE OverloadedStrings #-}
module Network.Wreq.Extras
    ( withSessionOpenSSL
    ) where

import           Network.Wreq.Session    (Session, withSessionControl)
import Network.HTTP.Client.OpenSSL
import OpenSSL.Session (context)
import Network.HTTP.Client (createCookieJar)
withSessionOpenSSL :: (Session -> IO a) -> IO a
withSessionOpenSSL = withOpenSSL . withSessionControl emptyCookieJar managerSettings
  where emptyCookieJar = Just (createCookieJar [])
        managerSettings = opensslManagerSettings context

