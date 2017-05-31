module NemoTest
    ( runTest
    ) where

import Graph
    ( Graph
    , fromList
    , toList
    , empty
    )
import Nemo
    ( Nemo
    , sync
    , successors
    )
import Util
    ( putShowLn
    )

a = "a"
b = "b"
c = "c"
d = "d"
e = "e"
someGraph = fromList [(a, [b, c]), (b, [d]), (c, [d]), (d, []), (e, [c])]
someNemo = (someGraph, empty :: Graph String, empty :: Graph String)

shadow _ k = k ++ "-clone"
test1 = toList deps
    where (deps, pred, clone) = sync shadow someNemo
test2 = toList deps
    where
        someNemo' = sync shadow someNemo
        (deps, pred, clone) = sync shadow someNemo'

someSuccessorGraph = fromList [(a, [b]), (b, [c]), (c, [e]), (d, [a])]
test3 = successors someSuccessorGraph a

runTest =
    putShowLn test1 >>
    putShowLn test2 >>
    putStrLn "" >>
    putShowLn test3
