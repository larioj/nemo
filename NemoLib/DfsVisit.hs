module NemoLib.DfsVisit where

import           Data.Set       (Set)
import           NemoLib.Cons
import           NemoLib.Elem
import           NemoLib.OrdSet
import           Prelude        hiding (elem)

dfsVisit :: Ord a => (a -> Set a) -> OrdSet a -> a -> OrdSet a
dfsVisit children progress node
    | elem node progress = progress
    | otherwise = cons node (foldl (dfsVisit children) progress (children node))
