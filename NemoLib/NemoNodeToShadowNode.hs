module NemoLib.NemoNodeToShadowNode where

import           NemoLib.GetAddress
import           NemoLib.GetContents
import           NemoLib.NemoNode
import           NemoLib.ReplaceDependencies
import           NemoLib.ShadowNode
import           NemoLib.ShiftedBase16Hash
import           Prelude                     hiding (getContents)

nemoNodeToShadowNode :: [(String, String)] -> NemoNode -> ShadowNode
nemoNodeToShadowNode pointers node =
    ShadowNode hash name contents
    where name = getAddress node
          preModuleContents = replaceDependencies pointers (getContents node)
          hash = shiftedBase16Hash preModuleContents
          contents = replaceDependencies [(hash, name)] preModuleContents
