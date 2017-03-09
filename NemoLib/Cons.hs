module NemoLib.Cons where

import           Data.Set       (insert)
import           NemoLib.OrdSet

cons :: Ord a => a -> OrdSet a -> OrdSet a
cons x (OrdSet s l) = OrdSet (x `insert` s) (x : l)
