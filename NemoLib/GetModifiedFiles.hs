module NemoLib.GetModifiedFiles where

import           Data.List          ((\\))
import           NemoLib.AsMap
import           NemoLib.ShadowNode

getModifiedFiles :: [ShadowNode] -> [ShadowNode] -> [String]
getModifiedFiles nemo domain =
    map snd $ (asMap nemo) \\ (asMap domain)
