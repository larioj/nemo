module NemoLib.ToSet where

import           NemoLib.OrdSet
import           Set            (Set)

toSet :: OrdSet a -> Set a
toSet (OrdSet s _) = s
