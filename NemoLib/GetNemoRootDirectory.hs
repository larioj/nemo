module NemoLib.GetNemoRootDirectory where

import System.Directory(getCurrentDirectory)
import NemoLib.FindNemoRootDirectory

getNemoRootDirectory :: IO (Maybe FilePath)
getNemoRootDirectory =
    getCurrentDirectory >>= \cd ->
    findNemoRootDirectory $ Just cd
        
