module NemoLib.Lookup where

import Prelude hiding(lookup)
import qualified Prelude (lookup)
import NemoLib.NemoNode
import NemoLib.Associate

lookup :: [NemoNode] -> FilePath -> Maybe NemoNode
lookup graph address = Prelude.lookup address (associate graph)