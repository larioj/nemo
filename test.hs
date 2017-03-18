#!/usr/bin/env runhaskell

import NemoLib.GetFile
import NemoLib.FileToNemoNode
import NemoLib.FlipFmap
import NemoLib.NemoNodesToShadowNodes

main = getFile "nemo.hs" $>> 
       fileToNemoNode $>> 
       ((flip (:)) []) $>>
       nemoNodesToShadowNodes >>= 
       print

