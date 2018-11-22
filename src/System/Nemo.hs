module System.Nemo where

import           Control.Monad    (unless)
import           System.Directory (createDirectory, doesDirectoryExist)

touchDirectory :: FilePath -> IO ()
touchDirectory path = do
  exists <- doesDirectoryExist path
  unless exists $ createDirectory path
