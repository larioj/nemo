module NemoLib.GetParentDirectory where

import System.FilePath.Posix

getParentDirectory :: FilePath -> Maybe FilePath
getParentDirectory "/" = Nothing
getParentDirectory p = Just $ takeDirectory p
