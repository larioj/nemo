module NemoLib.GetNemoDirectoryPath where

import           System.FilePath.Posix((</>))

getNemoDirectoryPath :: FilePath -> FilePath
getNemoDirectoryPath root = root </> "NemoLib"
