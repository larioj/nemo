module NemoLib.LookupDependencies where

import           Data.Set       (Set, fromList)
import           NemoLib.Lookup
import NemoLib.NemoNode
import NemoLib.Lookup
import Data.Maybe
import Prelude hiding (lookup)
import NemoLib.GetDependencies

lookupDependencies :: [NemoNode] -> NemoNode -> Set NemoNode
lookupDependencies graph node =
    fromList $ catMaybes $ map (lookup graph) (getDependencies node)
