module NemoLib.NemoNodesToShadowNodes where

import NemoLib.NemoNode
import NemoLib.ShadowNode
import NemoLib.NemoNodeToShadowNode
import NemoLib.TopoSort
import NemoLib.LookupDependencies
import NemoLib.FoldNemoNodeToShadowNode
import NemoLib.ExtractPointers
import NemoLib.ExtractPointer

nemoNodesToShadowNodes :: [NemoNode] -> [ShadowNode]
nemoNodesToShadowNodes nodes =
    foldl foldNemoNodeToShadowNode [] nodes
    where sorted = topoSort (lookupDependencies nodes) nodes

