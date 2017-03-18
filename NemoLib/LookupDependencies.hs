module NemoLib.LookupDependencies where

import           Data.Maybe
import           Data.Set                (Set, fromList)
import           NemoLib.GetDependencies
import           NemoLib.Lookup
import           NemoLib.Lookup
import           NemoLib.NemoNode
import           Prelude                 hiding (lookup)

lookupDependencies :: [NemoNode] -> NemoNode -> Set NemoNode
lookupDependencies graph node =
    fromList $ catMaybes $ map (lookup graph) (getDependencies node)
