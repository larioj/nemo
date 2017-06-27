module Nemo
    ( Nemo
    , sync
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
        }

sync :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> Nemo k v
sync cloneFn init@(Nemo rep g) =
    dfv seeds seen accfn init (dependencyGraph g)
    where
        seeds = Map.keysSet (dependencyGraph g)
        seen = Set.fromList $ catMaybes $ Map.elems (cloneGraph g)
        accfn = sync' cloneFn

sync' :: Ord k => (Nemo k v -> k -> (k, v)) -> Nemo k v -> k -> Nemo k v
sync' cloneFn old@(Nemo rep g) k = Nemo newRep newG
    where
        (clone, cloneRep) = cloneFn old k
        newRep = Map.insert clone cloneRep rep
        newG = update k clone g

empty :: Nemo k v
empty = Nemo Map.empty (NemoGraph Map.empty Map.empty Map.empty)
