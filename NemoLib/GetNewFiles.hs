module NemoLib.GetNewFiles where

import NemoLib.GetName
import Data.List((\\))
import NemoLib.ShadowNode

getNewFiles :: [ShadowNode] -> [ShadowNode] -> [String]
getNewFiles previous current =
    (map getName current) \\ (map getName previous)
