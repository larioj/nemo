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
    , update
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
    where (deps, pred, clone) = update shadow someNemo
test2 = toList deps
    where
        someNemo' = update shadow someNemo
        (deps, pred, clone) = update shadow someNemo'

runTest =
    putShowLn test1 >>= \_ ->
    putShowLn test2
