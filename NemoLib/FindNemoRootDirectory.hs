module NemoLib.FindNemoRootDirectory where

import NemoLib.IsNemoRootDirectory
import NemoLib.If
import NemoLib.GetParentDirectory

findNemoRootDirectory :: Maybe FilePath -> IO (Maybe FilePath)
findNemoRootDirectory Nothing = return Nothing
findNemoRootDirectory (Just p) =
    isNemoRootDirectory p >>= \isNemoRoot ->
    if' isNemoRoot
        (return (Just p))
        (findNemoRootDirectory (getParentDirectory p))
        
