module NemoGraph where

import Graph
    ( Graph
    )
import qualified Data.Set as Set
import Data.Set
    ( Set
    )
import Data.Maybe
    ( fromMaybe
    )
import qualified Data.Map as Map
import Data.Map
    ( Map
    , findWithDefault
    )

data NemoGraph k =
    NemoGraph
        { dependencyGraph  :: Graph k
        , predecessorGraph :: Map k (Maybe k)
        , cloneGraph       :: Map k (Maybe k)
        }

dependencies :: Ord k => NemoGraph k -> k -> Set k
dependencies g k =
    findWithDefault Set.empty k (dependencyGraph g)

predecessor :: Ord k => NemoGraph k -> k -> Maybe k
predecessor g k =
    findWithDefault Nothing k (predecessorGraph g)

clone :: Ord k => NemoGraph k -> k -> Maybe k
clone g k =
    findWithDefault Nothing k (cloneGraph g)

cloneOrSelf :: Ord k => NemoGraph k -> k -> k
cloneOrSelf g k = fromMaybe k (clone g k)

clonesDependencies :: Ord k => NemoGraph k -> k -> Set k
clonesDependencies g original =
    Set.map (cloneOrSelf g) (dependencies g original)

clonesPredecessor :: Ord k => NemoGraph k -> k -> Maybe k
clonesPredecessor g original =
    clone g original >>= predecessor g

insertDependencies :: Ord k => k
                            -> Set k
                            -> NemoGraph k
                            -> NemoGraph k
insertDependencies k deps g =
    g { dependencyGraph =
            Map.insert k deps (dependencyGraph g)}

insertPredecessor :: Ord k => k
                           -> Maybe k
                           -> NemoGraph k
                           -> NemoGraph k
insertPredecessor k pred g =
    if (Just k == pred) then g else
        g { predecessorGraph =
                Map.insert k pred (predecessorGraph g)}

insertClone :: Ord k => k
                     -> Maybe k
                     -> NemoGraph k
                     -> NemoGraph k
insertClone k clone g =
    g { cloneGraph =
            Map.insert k clone (cloneGraph g)}

update :: Ord k => k
                -> k
                -> NemoGraph k
                -> NemoGraph k
update k clone g =
    insertClone k (Just clone) .
    insertPredecessor clone (clonesPredecessor g k) .
    insertDependencies clone (clonesDependencies g k) $ g

