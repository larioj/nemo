module NemoLib.GetContents where

import           NemoLib.NemoNode

getContents :: NemoNode -> String
getContents (NemoNode _ c _) = c
