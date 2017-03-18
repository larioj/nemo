module NemoLib.Lookup where

import           NemoLib.Associate
import           NemoLib.NemoNode
import           Prelude           hiding (lookup)
import qualified Prelude           (lookup)

lookup :: [NemoNode] -> FilePath -> Maybe NemoNode
lookup graph address = Prelude.lookup address (associate graph)
