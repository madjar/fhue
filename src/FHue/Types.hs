{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FHue.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char     (toLower)

data FileType = Dir | File deriving Show

$(deriveJSON defaultOptions {sumEncoding = ObjectWithSingleField, constructorTagModifier = map toLower} ''FileType)

data Item = Item { itemName  :: String
                 , itemType  :: FileType
                 , itemRwx   :: String
                 , itemMtime :: String
                 , itemSize  :: Int
                 , itemUser  :: String
                 , itemGroup :: String
                 } deriving Show

instance FromJSON Item where
    parseJSON (Object v) = do stats <- v .: "stats"
                              Item <$>
                                v .: "name" <*>
                                v .: "type" <*>
                                v .: "rwx" <*>
                                v .: "mtime" <*>
                                stats .: "size" <*>
                                stats .: "user" <*>
                                stats .: "group"
    parseJSON _ = mempty

class Monad m => MonadHdfs m where
  list :: String -> m [Item]
