module NemoLib.ExtractPointers where

import           NemoLib.ExtractPointer
import           NemoLib.ShadowNode

extractPointers :: [ShadowNode] -> [(String, String)]
extractPointers = map extractPointer

