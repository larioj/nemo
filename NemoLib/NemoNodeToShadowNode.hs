module NemoLib.NemoNodeToShadowNode where

import NemoLib.NemoNode
import NemoLib.ShadowNode
import NemoLib.GetContents
import NemoLib.GetAddress
import NemoLib.ReplaceDependencies
import NemoLib.ShiftedBase16Hash
import Prelude hiding (getContents)

-- nemo to hash
nemoNodeToShadowNode :: [(String, String)] -> NemoNode -> ShadowNode
nemoNodeToShadowNode pointers node =
    ShadowNode address (getAddress node) contents
    where contents = replaceDependencies pointers (getContents node)
          address = shiftedBase16Hash contents
