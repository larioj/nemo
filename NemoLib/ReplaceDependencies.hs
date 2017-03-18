module NemoLib.ReplaceDependencies where

import           NemoLib.ReplaceDependency

replaceDependencies :: [(String, String)] -> String -> String
replaceDependencies pointers contents =
    foldl replaceDependency contents pointers

