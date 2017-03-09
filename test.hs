#!/usr/bin/env runhaskell

import NemoLib.ExtractDependencies

main = readFile "nemo.hs" >>= \c ->
       print (extractDependencies c)

