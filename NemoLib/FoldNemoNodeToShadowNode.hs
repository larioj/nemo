module NemoLib.FoldNemoNodeToShadowNode where

import           NemoLib.ExtractPointers
import           NemoLib.NemoNode
import           NemoLib.NemoNodeToShadowNode
import           NemoLib.ShadowNode

foldNemoNodeToShadowNode :: [ShadowNode] -> NemoNode -> [ShadowNode]
foldNemoNodeToShadowNode sNodes nemoNode = sNode : sNodes
    where pointers = extractPointers sNodes
          sNode = nemoNodeToShadowNode pointers nemoNode
