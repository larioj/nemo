module Nemo where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Map
    ( Map
    )
import Graph
    ( Graph
    , dfv
    )
import Data.Maybe
    ( catMaybes
    )
import NemoGraph
    ( NemoGraph (..)
    , dependencyGraph
    , cloneGraph
    , update
    )

data Nemo k v =
    Nemo
        { representationMap :: Map k v
        , nemoGraph         :: NemoGraph k
        } deriving (Show, Eq)

sync :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> Nemo k v
sync cloneFn init@(Nemo rep g) =
    dfv seeds seen accfn init (dependencyGraph g)
    where
        seeds = Map.keysSet (dependencyGraph g)
        seen = Set.fromList $ catMaybes $ Map.elems (cloneGraph g)
        accfn = sync' cloneFn

sync' :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> k -> Nemo k v
sync' cloneFn old@(Nemo rep g) k = new
    where
        (clone, cloneRep) = cloneFn old k
        newRep = Map.insert clone cloneRep rep
        newG = update k clone g
        new = Nemo newRep newG

empty :: Nemo k v
empty = Nemo Map.empty (NemoGraph Map.empty Map.empty Map.empty)
