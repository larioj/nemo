module NemoLib.Elem where

import           Data.Set       (member)
import           NemoLib.OrdSet

elem :: Ord a => a -> OrdSet a -> Bool
elem x (OrdSet xs _) = x `member` xs
