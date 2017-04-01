module NemoLib.GetShadowDirectoryPath where

import NemoLib.GetNemoDirectoryPath
import           System.FilePath.Posix((</>))

getShadowDirectoryPath ::  FilePath -> FilePath
getShadowDirectoryPath root = (getNemoDirectoryPath root) </> "NemoLib"
