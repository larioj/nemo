{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphTest
    ( runTest
    )where

import Data.Set
    ( Set
    , fromList
    )

import Graph
    ( Graph
    , vertices
    , children
    , topoSort
    )

import Data.Maybe
    ( maybeToList
    )

instance Graph [(Int, [Int])] Int (Int, Int) where
    vertices = fromList . fmap fst
    children g v = fromList . concat . maybeToList . lookup v $ g

pairs :: (Int, [Int]) -> Set (Int, Int)
pairs (i, s) = fromList . fmap (\s -> (i, s)) $ s

someGraph :: [(Int, [Int])]
someGraph = [(1, [2, 4]), (2, [3]), (3, []), (4, [3])]

runTest = putStrLn . show $ (topoSort someGraph)
