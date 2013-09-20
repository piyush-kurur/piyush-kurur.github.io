{-# LANGUAGE DeriveDataTypeable  #-}

module Site.RemoteFile
       ( copyRemoteFileCompiler
       ) where

import Data.Binary
import Data.Typeable
import Hakyll
import System.Directory(copyFile)

data RemoteFile = RemoteFile { unRemoteFile :: FilePath }
                  deriving (Show, Eq, Ord, Typeable)

instance Binary RemoteFile where
  get    = fmap RemoteFile $ get
  put    = put . unRemoteFile

instance Writable RemoteFile where
  write dest item = copyFile (unRemoteFile $ itemBody item) dest


copyRemoteFileCompiler :: FilePath -> Compiler (Item RemoteFile)
copyRemoteFileCompiler = makeItem . RemoteFile
