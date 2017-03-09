module NemoLib.ToList where

import           NemoLib.OrdSet

toList :: OrdSet a -> [a]
toList (OrdSet _ l) = l
