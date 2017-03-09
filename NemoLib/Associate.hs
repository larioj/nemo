module NemoLib.Associate where

import NemoLib.NemoNode
import NemoLib.GetAddress

associate :: [NemoNode] -> [(FilePath, NemoNode]
associate = map (\n -> (getAddress n, n))