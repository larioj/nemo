#!/usr/bin/env runhaskell

import NemoLib.GetFile
import NemoLib.FileToNemoNode
import NemoLib.FlipFmap
import NemoLib.NemoNodesToShadowNodes
import NemoLib.NemoNode
import NemoLib.FoldNemoNodeToShadowNode
import NemoLib.ShadowNode
import NemoLib.TopoSort
import NemoLib.LookupDependencies

main2 =
    print (topoSort (lookupDependencies nodes) nodes)

main1 =
    print (foldNemoNodeToShadowNode step1 fooN)


main =
    print (nemoNodesToShadowNodes [ifN, fooN])



ifN = NemoNode "If" "module NemoLib.If where\n" []
fooN = NemoNode "Foo" "module NemoLib.Foo where \n import NemoLib.If \n\nimport NemoLib.If\n" ["If"]


nodes = [ifN, fooN]

step1 = [ShadowNode "JDKDBHKNKHAFIAOHLFABJGHNDKKEGLAMCAHPMDPGHAOPIDDCPNPGFJCDAOGHFGAC" "If" "module NemoLib.ShadowLib.JDKDBHKNKHAFIAOHLFABJGHNDKKEGLAMCAHPMDPGHAOPIDDCPNPGFJCDAOGHFGAC_If where\n"]