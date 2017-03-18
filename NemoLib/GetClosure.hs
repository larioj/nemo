module NemoLib.GetClosure where

import           Data.Maybe                 (fromJust)
import           NemoLib.DfsVisit
import           NemoLib.FromList
import           NemoLib.Lookup
import           NemoLib.LookupDependencies
import           NemoLib.NemoNode
import           NemoLib.ToList
import           Prelude                    hiding (lookup)

getClosure :: String -> [NemoNode] -> [NemoNode]
getClosure name graph =
    toList (dfsVisit (lookupDependencies graph) (fromList []) (fromJust (lookup graph name)))
