module NemoLib.LookupDependencies where

import Data.Set(Set, fromList)
import NemoLib.Lookup

lookupDependencies :: [NemoNode] -> NemoNode -> Set NemoNode
lookupDependencies graph node =
    fromList $ catMaybes $ map (lookup graph) (dependencies node)
