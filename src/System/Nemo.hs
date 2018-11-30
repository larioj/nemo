module System.Nemo where

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           System.Directory       (createDirectory, doesDirectoryExist)
import           System.Posix.Files     (setFileMode)

touchDirectory :: FilePath -> IO ()
touchDirectory path = do
  exists <- doesDirectoryExist path
  unless exists $ createDirectory path

makeReadOnly :: MonadIO m => FilePath -> m ()
makeReadOnly path = liftIO $ setFileMode path 292
