module Nemo where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set   as Set
import           Graph      (dfv)
import           NemoGraph  (NemoGraph (..), cloneGraph, dependencyGraph,
                             update)

data Nemo k v =
    Nemo
        { representationMap :: Map k v
        , nemoGraph         :: NemoGraph k
        } deriving (Show, Eq)

update :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> Nemo k v
update cloneFn init@(Nemo _ g) =
    dfv seeds seen accfn init (dependencyGraph g)
    where
        seeds = Map.keysSet (dependencyGraph g)
        seen = Set.fromList $ catMaybes $ Map.elems (cloneGraph g)
        accfn = update' cloneFn

update' :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> k -> Nemo k v
update' cloneFn old@(Nemo rep g) k = new
    where
        (clone, cloneRep) = cloneFn old k
        newRep = Map.insert clone cloneRep rep
        newG = NemoGraph.update k clone g
        new = Nemo newRep newG

empty :: Nemo k v
empty = Nemo Map.empty (NemoGraph Map.empty Map.empty Map.empty)
