module NemoLib.GetNemoFilePath where

import           System.FilePath.Posix((</>))

getNemoFilePath :: FilePath -> FilePath
getNemoFilePath root = root </> ".nemo"
