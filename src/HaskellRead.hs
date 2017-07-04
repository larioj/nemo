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
import Util
import NemoGraph
import Graph
import System.FilePath.Posix (takeExtension)

haskellNemo :: [File] -> Nemo String File
haskellNemo files =
    Nemo reps NemoGraph.empty {
        dependencyGraph = deps,
        cloneGraph = Map.empty,
        predecessorGraph = Map.empty
    }
    where
        haskellFiles = selectHaskellFiles files
        reps = Map.fromList $ Prelude.map (\f -> (identifier f, f)) haskellFiles
        deps = Graph.graph $ Prelude.map (\f -> (identifier f, extractDependencies f)) haskellFiles

selectHaskellFiles :: [File] -> [File]
selectHaskellFiles = select isHaskellFile

selectHaskellPaths :: [FilePath] -> [FilePath]
selectHaskellPaths paths =
    select ((==) ".hs" . takeExtension) paths

isHaskellFile :: File -> Bool
isHaskellFile file =
    (extension file) == ".hs"

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
