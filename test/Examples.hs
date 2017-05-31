module Examples where

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

graphA = fromList
    [ (a, [b, c])
    ,     (b, [d])
    ,     (c, [d])
    ,         (d, [])
    , (e, [c])
    ]

nemoA =
    ( graphA
    , empty :: Graph String
    , empty :: Graph String
    )
