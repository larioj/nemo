module NemoLib.FileToNemoNode where

import NemoLib.ExtractDependencies

fileToNemoNode :: File -> NemoNode
fileToNemoNode (File path contents) =
    NemoNode path contents dependencies
    where dependencies = 
        (map (\p -> p ++ ".hs") (extractDependencies contents))
