#!/usr/bin/env runhaskell

import           Data.List  (nub)
import           Data.Maybe (fromJust)
import           Data.Set(Set)
import qualified Data.Set as Set
import           OrdSet     (OrdSet)
import qualified OrdSet

main = print (topoSort getChildren (map fst graph))


a = ("a", [])
b = ("b", ["a"])
c = ("c", ["b"])
d = ("d", ["c", "b"])
e = ("e", ["b"])

graph = [b, a, d, c, e]

type Node = (String, [String])
type Graph = [Node]


getChildren :: String -> Set String
getChildren node = (Set.fromList .fromJust) (lookup node graph)


dfsVisit :: Ord a => (a -> Set a) -> OrdSet a -> a -> OrdSet a
dfsVisit children progress node
    | OrdSet.elem node progress = progress
    | otherwise = OrdSet.cons node (foldl (dfsVisit children) progress (children node))

topoSort :: Ord a => (a -> Set a) -> [a] -> [a]
topoSort children nodes =
    OrdSet.list (foldl (dfsVisit children) (OrdSet.fromList []) nodes)




{--
dfsVisit :: Eq a => (a -> [a]) -> [a] -> a -> [a]
dfsVisit children progress node
    | node `elem` progress = progress
    | otherwise = node : (foldl (dfsVisit children) progress (children node))

topoSort :: Eq a => (a -> [a]) -> [a] -> [a]
topoSort children nodes =
    foldl (dfsVisit children) [] nodes
--}

{--
dfsVisit :: Graph -> [String] -> String -> [String]
dfsVisit graph progress node
    | node `elem` progress = progress
    | otherwise = node : (foldl (dfsVisit graph) progress (fromJust (lookup node graph)))

topoSort :: Graph -> [String]
topoSort graph = foldl (dfsVisit graph) [] (map fst graph)
--}

{--
dfsVisit :: Graph -> String -> [String]
dfsVisit graph start =
    start : (foldl (\seen start -> (dfsVisit graph start) ++ seen) [] (fromJust (lookup start graph)))

topoSort graph start =
    (nub . reverse) (dfsVisit graph start)
--}



