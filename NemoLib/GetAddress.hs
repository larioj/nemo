module NemoLib.GetAddress where

import NemoLib.NemoNode

getAddress :: NemoNode -> FilePath
getAddress (NemoNode n _ _) = n
