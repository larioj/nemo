module NemoLib.TopoSort where

import           Data.Set         (Set)
import           NemoLib.DfsVisit
import           NemoLib.FromList
import           NemoLib.ToList

topoSort :: Ord a => (a -> Set a) -> [a] -> [a]
topoSort children nodes =
    toList (foldl (dfsVisit children) (fromList []) nodes)
