module GraphTest
    ( runTest
    )where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Graph
    ( Graph
    , topoSort
    , fromList
    , toList
    , inverse
    )
import Util
    ( putShowLn
    )

someGraph :: Graph Int
someGraph = fromList [(1, [2, 4]), (2, [3]), (3, []), (4, [3]), (5, [])]

test1 = topoSort someGraph
test2 = toList . inverse $ someGraph

runTest =
    putShowLn test1 >>
    putShowLn (toList someGraph) >>
    putShowLn test2
