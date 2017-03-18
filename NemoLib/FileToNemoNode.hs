module NemoLib.FileToNemoNode where

import NemoLib.ExtractDependencies
import NemoLib.PathToAddress
import NemoLib.NemoNode
import NemoLib.File

fileToNemoNode :: File -> NemoNode
fileToNemoNode (File path contents) =
    NemoNode address contents (extractDependencies contents)
    where address = pathToAddress path
