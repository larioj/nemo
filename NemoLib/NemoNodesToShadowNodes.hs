module NemoLib.NemoNodesToShadowNodes where

import           NemoLib.ExtractPointer
import           NemoLib.ExtractPointers
import           NemoLib.FoldNemoNodeToShadowNode
import           NemoLib.LookupDependencies
import           NemoLib.NemoNode
import           NemoLib.NemoNodeToShadowNode
import           NemoLib.ShadowNode
import           NemoLib.TopoSort

nemoNodesToShadowNodes :: [NemoNode] -> [ShadowNode]
nemoNodesToShadowNodes nodes =
    foldl foldNemoNodeToShadowNode [] sorted
    where sorted = topoSort (lookupDependencies nodes) nodes

