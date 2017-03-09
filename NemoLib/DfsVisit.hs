module NemoLib.DfsVisit where

import           Data.Set (Set)
import           OrdSet   (OrdSet)
import qualified OrdSet

dfsVisit :: Ord a => (a -> Set a) -> OrdSet a -> a -> OrdSet a
dfsVisit children progress node
    | OrdSet.elem node progress =
        progress
    | otherwise =
        OrdSet.cons node (foldl (dfsVisit children) progress (children node))
