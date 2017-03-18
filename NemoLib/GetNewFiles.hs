module NemoLib.GetNewFiles where

import           Data.List          ((\\))
import           NemoLib.GetName
import           NemoLib.ShadowNode

getNewFiles :: [ShadowNode] -> [ShadowNode] -> [String]
getNewFiles previous current =
    (map getName current) \\ (map getName previous)
