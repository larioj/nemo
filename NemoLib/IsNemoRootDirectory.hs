module NemoLib.IsNemoRootDirectory where

import NemoLib.GetNemoFilePath
import NemoLib.GetNemoDirectoryPath
import NemoLib.GetShadowDirectoryPath
import System.Directory(doesFileExist, doesDirectoryExist)

isNemoRootDirectory :: FilePath -> IO Bool
isNemoRootDirectory p =
    doesFileExist nemoFile >>= \nemoFileExists ->
    doesDirectoryExist nemoDirectory >>= \nemoDirectoryExists ->
    doesDirectoryExist shadowDirectory >>= \shadowDirectoryExists ->
    return (nemoFileExists && nemoDirectoryExists && shadowDirectoryExists)
    where nemoFile = getNemoFilePath p
          nemoDirectory = getNemoDirectoryPath p
          shadowDirectory = getShadowDirectoryPath p
    
