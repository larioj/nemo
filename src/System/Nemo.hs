module System.Nemo where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Directory       (createDirectory, doesDirectoryExist,
                                         doesPathExist)
import           System.Directory       (copyFile, renameFile)
import           System.Posix.Files     (setFileMode)
import           System.Posix.Files     (createSymbolicLink)

createDirectoryIfNotExists :: FilePath -> IO ()
createDirectoryIfNotExists path = do
  exists <- doesDirectoryExist path
  unless exists $ createDirectory path

makeReadOnly :: MonadIO m => FilePath -> m ()
makeReadOnly path = liftIO $ setFileMode path 292

copyIfNotExists :: FilePath -> FilePath -> IO ()
copyIfNotExists source dest = do
  destExists <- doesPathExist dest
  unless destExists $ copyFile source dest

moveIfNotExists :: FilePath -> FilePath -> IO ()
moveIfNotExists source dest = do
  destExists <- doesPathExist dest
  unless destExists $ renameFile source dest

symLinkIfNotExists :: FilePath -> FilePath -> IO ()
symLinkIfNotExists dest link = do
  linkExists <- doesPathExist link
  unless linkExists $ createSymbolicLink dest link
