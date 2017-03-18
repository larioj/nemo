module NemoLib.AsMap where

import NemoLib.ShadowNode

asMap :: [ShadowNode] -> [(String, String)]
asMap = map (\(ShadowNode hash name _) -> (hash, name)) 