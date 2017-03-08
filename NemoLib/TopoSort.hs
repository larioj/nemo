module NemoLib.NemoNode(topoSort) where

import           NemoLib.File
import           NemoLib.NemoNode

main = putStrLn "hello"

topoSort :: [NemoNode] -> [NemoNode]
topoSort [] = []
topoSort (first : rest) =
    (topoSort (rest \\ suffix)) ++ suffix
