module NemoLib.Lookup where

import Prelude hiding(lookup)
import qualified Prelude (lookup)
import NemoLib.NemoNode

lookup :: [NemoNode] -> FilePath -> NemoNode
lookup graph address = Prelude.lookup path (associate graph)