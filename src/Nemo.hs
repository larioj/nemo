module Nemo
    ( Nemo
    , update
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
    )

type Nemo k = (Graph k, Graph k, Graph k)

update :: Ord k => (Nemo k -> k -> k) -> Nemo k -> Nemo k
update f (dependencyGraph, predecessorGraph, cloneGraph) =
    dfv seeds ctl accfn init dependencyGraph
    where
        seeds = Map.keysSet dependencyGraph
        ctl = Map.keysSet cloneGraph
        accfn = update' f
        init = (dependencyGraph, predecessorGraph,cloneGraph)

update' :: Ord k => (Nemo k -> k -> k) -> Nemo k -> k -> Nemo k
update' f (dependencyGraph, predecessorGraph, cloneGraph) original =
    (newDependencyGraph, newPredecessorGraph, newCloneGraph)
    where
        originalDependencies = findWithDefault Set.empty original dependencyGraph
        clone = f (dependencyGraph, predecessorGraph, cloneGraph) original
        cloneDependencies = Set.map (getDependencyClone cloneGraph) originalDependencies
        newDependencyGraph = Map.insert clone cloneDependencies dependencyGraph
        newCloneGraph = Map.insert original (Set.singleton clone) cloneGraph
        oldClone = findWithDefault Set.empty original cloneGraph
        newPredecessorGraph = Map.insert clone oldClone predecessorGraph

getDependencyClone :: Ord k => Graph k -> k -> k
getDependencyClone cloneGraph dep =
    head . Set.elems . findWithDefault (Set.singleton dep) dep $ cloneGraph
