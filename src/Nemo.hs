module Nemo
    ( Nemo
    , sync
    , successors
    ) where

import qualified Data.Set as Set
import Data.Set
    ( Set
    )
import qualified Data.Map as Map
import Data.Map
    ( findWithDefault
    )
import Graph
    ( Graph
    , dfv
    , inverse
    )
import Util
    ( if'
    )

type Nemo k = (Graph k, Graph k, Graph k)

sync :: Ord k => (Nemo k -> k -> k) -> Nemo k -> Nemo k
sync f (dependencyGraph, predecessorGraph, cloneGraph) =
    dfv seeds ctl accfn init dependencyGraph
    where
        seeds = Map.keysSet dependencyGraph
        ctl = Set.fromList . concat . map Set.toList . Map.elems $ cloneGraph
        accfn = sync' f
        init = (dependencyGraph, predecessorGraph,cloneGraph)

sync' :: Ord k => (Nemo k -> k -> k) -> Nemo k -> k -> Nemo k
sync' f (dependencyGraph, predecessorGraph, cloneGraph) original =
    if' ((Set.singleton clone) == oldClone) sameState changedState
    where
        sameState = (dependencyGraph, predecessorGraph, cloneGraph)
        originalDependencies = findWithDefault Set.empty original dependencyGraph
        clone = f (dependencyGraph, predecessorGraph, cloneGraph) original
        oldClone = findWithDefault Set.empty original cloneGraph
        cloneDependencies = Set.map (getDependencyClone cloneGraph) originalDependencies
        newDependencyGraph = Map.insert clone cloneDependencies dependencyGraph
        newCloneGraph = Map.insert original (Set.singleton clone) cloneGraph
        newPredecessorGraph = Map.insert clone oldClone predecessorGraph
        changedState = (newDependencyGraph, newPredecessorGraph, newCloneGraph)

getDependencyClone :: Ord k => Graph k -> k -> k
getDependencyClone cloneGraph dep =
    head . Set.elems . findWithDefault (Set.singleton dep) dep $ cloneGraph

upgrade :: Ord k => Nemo k -> k -> k -> Maybe (Nemo k)
upgrade (dependencyGraph, predecessorGraph, cloneGraph) old new =
    if' (elem new succs) (Just newState) Nothing
    where
        successorGraph = inverse predecessorGraph
        succs = successors successorGraph old
        newState = undefined -- TODO: change the state given the new version

successors :: Ord k => Graph k -> k -> [k]
successors g k = dfv (Set.singleton k) Set.empty (flip (:)) [] g
