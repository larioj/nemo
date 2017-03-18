module NemoLib.ReplaceDependencies where

import           NemoLib.ReplaceDependency

-- replace NemoLib.<key> with NemoLib.ShadowLib.<value>

replaceDependencies :: [(String, String)] -> String -> String
replaceDependencies pointers contents =
    foldl replaceDependency contents pointers

