module HaskellRead where

import Nemo
import File
import EscapeRegex
import NemoGraph
import Text.Regex.Posix
import Data.List.Utils (replace)
import Data.Set as Set
import Data.Map as Map
import Hash
import HaskellTransform

moduleToIdentifier :: String -> String
moduleToIdentifier mod =
    replace "." "/" mod ++ ".hs"

extractDependencies :: File -> [String]
extractDependencies file =
    undefined

