module NemoLib.Associate where

import           NemoLib.GetAddress
import           NemoLib.NemoNode

associate :: [NemoNode] -> [(FilePath, NemoNode)]
associate = map (\n -> (getAddress n, n))
