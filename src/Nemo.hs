module Nemo
    ( Nemo
    , sync
    , successors
    , getDependencyClone
    , empty
    ) where

import qualified Data.Set as Set
import Data.Set
    ( Set
    )
import qualified Data.Map as Map
import Data.Map
    ( Map
    , findWithDefault
    )
import Graph
    ( Graph
    , dfv
    )
import Util
    ( if'
    )

type Nemo k v = (Map k v, Graph k, Graph k, Graph k)

sync :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> Nemo k v
sync f (representationMap, dependencyGraph, predecessorGraph, cloneGraph) =
    dfv seeds ctl accfn init dependencyGraph
    where
        seeds = Map.keysSet dependencyGraph
        ctl = Set.fromList . concat . map Set.toList . Map.elems $ cloneGraph
        accfn = sync' f
        init = (representationMap, dependencyGraph, predecessorGraph, cloneGraph)

sync' :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> k -> Nemo k v
sync' f (representationMap, dependencyGraph, predecessorGraph, cloneGraph) original =
    if' ((Set.singleton clone) == oldClone) sameState changedState
    where
        sameState = (representationMap, dependencyGraph, predecessorGraph, cloneGraph)
        originalDependencies = findWithDefault Set.empty original dependencyGraph
        (clone, representation) = f sameState original
        oldClone = findWithDefault Set.empty original cloneGraph
        cloneDependencies = Set.map (getDependencyClone cloneGraph) originalDependencies
        newDependencyGraph = Map.insert clone cloneDependencies dependencyGraph
        newCloneGraph = Map.insert original (Set.singleton clone) cloneGraph
        newPredecessorGraph = Map.insert clone oldClone predecessorGraph
        newRepresentationMap = Map.insert clone representation representationMap
        changedState = (newRepresentationMap, newDependencyGraph, newPredecessorGraph, newCloneGraph)

getDependencyClone :: Ord k => Graph k -> k -> k
getDependencyClone cloneGraph dep =
    head . Set.elems . findWithDefault (Set.singleton dep) dep $ cloneGraph

successors :: Ord k => Graph k -> k -> [k]
successors g k = dfv (Set.singleton k) Set.empty (flip (:)) [] g

empty :: Nemo k v
empty = (Map.empty, Map.empty, Map.empty, Map.empty)
