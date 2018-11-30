module Data.List.Stack where

import           Control.Lens (Lens', lens)

push :: a -> [a] -> [a]
push = (:)

pop :: [a] -> [a]
pop = tail

peek :: Lens' [a] a
peek = lens head (\l a -> a : tail l)
