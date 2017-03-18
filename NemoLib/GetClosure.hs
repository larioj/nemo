module NemoLib.GetClosure where

import NemoLib.NemoNode
import NemoLib.ToList
import NemoLib.FromList
import NemoLib.LookupDependencies
import           NemoLib.Lookup
import Data.Maybe(fromJust)
import Prelude hiding (lookup)
import NemoLib.DfsVisit

getClosure :: String -> [NemoNode] -> [NemoNode]
getClosure name graph =
    toList (dfsVisit (lookupDependencies graph) (fromList []) (fromJust (lookup graph name)))
