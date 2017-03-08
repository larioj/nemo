module OrdSet where

import           Data.Set (Set, insert, member)
import qualified Data.Set as Set (fromList)

data OrdSet a = OrdSet { set  :: (Set a)
                       , list :: [a]
                       } deriving (Show, Eq, Ord)

cons :: Ord a => a -> OrdSet a -> OrdSet a
cons x (OrdSet s l) = OrdSet (x `insert` s) (x : l)

fromList :: Ord a => [a] -> OrdSet a
fromList xs = OrdSet (Set.fromList xs) xs

elem :: Ord a => a -> OrdSet a -> Bool
elem x (OrdSet xs _) = x `member` xs
