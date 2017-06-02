module Examples where

import Graph
    ( Graph
    , graph
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

graphA = graph
    [ (a, [b, c])
    ,     (b, [d])
    ,     (c, [d])
    ,         (d, [])
    , (e, [c])
    ]

graphAInverse =
    graph
        [ (a, [])
        , (b, [a])
        , (c, [a, e])
        , (d, [b, c])
        , (e, [])
        ]

nemoA =
    ( graphA
    , empty :: Graph String
    , empty :: Graph String
    )

graphB = graph [(a, [b]), (b, [c]), (c, [e]), (d, [a])]

aSuccessorsGraphB = [a, b, c, e]
