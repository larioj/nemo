{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module GraphTest
    ( runTest
    )where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Graph
    ( Graph
    , topoSort
    )

fromList :: Ord k => [(k, [k])] -> Graph k
fromList = Map.map Set.fromList . Map.fromList

someGraph :: Graph Int
someGraph = fromList [(1, [2, 4]), (2, [3]), (3, []), (4, [3])]

runTest = putStrLn . show $ (topoSort someGraph)
