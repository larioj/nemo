module NemoLib.PathToAddress where

import           System.FilePath.Posix (takeBaseName)

-- drop the .hs extension, and remove
pathToAddress :: String -> String
pathToAddress = takeBaseName
