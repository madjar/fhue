{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FHue.FakeHdfs where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           System.Console.Haskeline.MonadException

import           FHue.Types
import           FHue.Class

-- TODO : use a tree, not a list
newtype FakeHdfsT m a = FakeHdfsT {
    extractFakeHdfsT :: ReaderT [Item] m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadException)

-- deriving instance MonadException m => MonadException (FakeHdfsT m)

instance Monad m => MonadHdfs (FakeHdfsT m) where
  list _ = FakeHdfsT ask -- TODO don't return everything, return what matches path
  upload = undefined
  download = undefined
  remove = undefined

type FakeHdfs = FakeHdfsT Identity

runFakeHdfsT :: FakeHdfsT m a -> [Item] -> m a
runFakeHdfsT f = runReaderT (extractFakeHdfsT f)

runFakeHdfs :: FakeHdfs a -> [Item] -> a
runFakeHdfs f = runIdentity . runFakeHdfsT f


mkItem :: FileType -> String -> Item
mkItem fileType name = Item name fileType "" "" 0 "" ""

mkFile = mkItem File
mkDir = mkItem Dir

