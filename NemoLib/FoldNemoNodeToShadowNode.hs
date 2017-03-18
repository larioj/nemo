module NemoLib.FoldNemoNodeToShadowNode where

import NemoLib.ShadowNode
import NemoLib.NemoNode
import NemoLib.ExtractPointers
import NemoLib.NemoNodeToShadowNode

foldNemoNodeToShadowNode :: [ShadowNode] -> NemoNode -> [ShadowNode]
foldNemoNodeToShadowNode sNodes nemoNode = sNode : sNodes
    where pointers = extractPointers sNodes
          sNode = nemoNodeToShadowNode pointers nemoNode
