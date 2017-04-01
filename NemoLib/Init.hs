module NemoLib.Init where

import Prelude hiding(init)
import NemoLib.GetNemoDirectoryPath
import NemoLib.GetNemoFilePath
import NemoLib.GetShadowDirectoryPath
import           System.Directory(createDirectory, getCurrentDirectory)

init :: IO ()
init =
    getCurrentDirectory >>= \root ->
    createDirectory (getNemoDirectoryPath root) >>
    createDirectory (getShadowDirectoryPath root) >>
    writeFile (getNemoFilePath root) "[]"
