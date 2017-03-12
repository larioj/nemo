module NemoLib.FileToNemoNode where

import NemoLib.ExtractDependencies

fileToNemoNode :: File -> NemoNode
fileToNemoNode (File path contents) =
    NemoNode path contents (extractDependencies contents)
