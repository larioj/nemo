module NemoLib.GetModifiedFiles where

import NemoLib.ShadowNode
import NemoLib.AsMap
import Data.List((\\))
     
getModifiedFiles :: [ShadowNode] -> [ShadowNode] -> [String]
getModifiedFiles nemo domain =
    map snd $ (asMap nemo) \\ (asMap domain)
