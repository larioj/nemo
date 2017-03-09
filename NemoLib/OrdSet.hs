module NemoLib.OrdSet where

import           Data.Set (Set)

data OrdSet a =
     OrdSet (Set a) [a]
     deriving (Show, Eq, Ord)
