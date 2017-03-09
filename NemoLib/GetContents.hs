module NemoLib.GetContents where

getContents :: NemoNode -> String
getContents (NemoNode _ c _) = c
