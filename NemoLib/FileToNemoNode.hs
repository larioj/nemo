module NemoLib.FileToNemoNode where

import           NemoLib.ExtractDependencies
import           NemoLib.File
import           NemoLib.NemoNode
import           NemoLib.PathToAddress

fileToNemoNode :: File -> NemoNode
fileToNemoNode (File path contents) =
    NemoNode address contents (extractDependencies contents)
    where address = pathToAddress path
