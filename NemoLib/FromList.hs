module NemoLib.FromList where

import qualified Data.Set as Set (fromList)

fromList :: Ord a => [a] -> OrdSet a
fromList xs = OrdSet (Set.fromList xs) xs
