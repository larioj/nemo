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

extractDependencies :: File -> [String]
extractDependencies file =
    Prelude.map (moduleToIdentifier . captureModule) imports
    where
        imports = extractImportExpressions file

extractImportExpressions :: File -> [String]
extractImportExpressions file =
    getAllTextMatches $ (contents file) =~ importRegex

importRegex :: String
importRegex = "import([\r\n\t\f\v ]+qualified)?[\r\n\t\f\v ]+([^\r\n\t\f\v ]+)"

captureModule :: String -> String
captureModule importLine = mod
    where
        match = importLine =~ importRegex :: (String, String, String, [String])
        (_, _, _, captures) = match
        mod = captures !! 1

moduleToIdentifier :: String -> String
moduleToIdentifier mod =
    replace "." "/" mod ++ ".hs"
