module Read where

import File
import Directory
import NemoConfig
import System.FilePath.Posix
import Data.Map as Map
import HaskellRead
import Util
import Data.List (isInfixOf)
import Nemo
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)
import Graph
import NemoGraph

getNemo :: FilePath -> IO (Nemo String File)
getNemo root =
    getAllFiles root >>= \files ->
    getDependencyGraph root files >>= \deps ->
    getCloneGraph root >>= \clones ->
    getPredecessorGraph root >>= \preds ->
        return $ Nemo (toRepresentation files) $ NemoGraph {
            dependencyGraph = deps,
            cloneGraph = clones,
            predecessorGraph = preds
        }

getIgnoreSpec :: FilePath -> IO [String]
getIgnoreSpec root =
    readListFile root ignoreFile

getAllSources :: FilePath -> IO [FilePath]
getAllSources root =
    getIgnoreSpec root >>= \spec ->
    listDirectoryRecursively root >>= \paths ->
        return $ selectSupportedPaths $ ignorePaths root spec paths

getAllFiles :: FilePath -> IO [File]
getAllFiles root =
    getAllSources root >>= loadAll root

getCloneGraph :: FilePath -> IO (Map String (Maybe String))
getCloneGraph root = getMap root cloneFile

getPredecessorGraph :: FilePath -> IO (Map String (Maybe String))
getPredecessorGraph root = getMap root predecessorFile

selectSupportedPaths :: [FilePath] -> [FilePath]
selectSupportedPaths = selectHaskellPaths

selectSupportedFiles :: [File] -> [File]
selectSupportedFiles = selectHaskellFiles

-- TODO: will multiplex file types
extractDependencies :: File -> [FilePath]
extractDependencies = HaskellRead.extractDependencies

getDependencies :: FilePath -> File -> IO [FilePath]
getDependencies root file =
    fmap catMaybes $ sequence $
    (flip fmap) (Read.extractDependencies file) $ \dep ->
        ifM (doesFileExist $ root </> dep)
            (return $ Just dep)
            (return Nothing)

getDependencyGraph :: FilePath -> [File] -> IO (Graph String)
getDependencyGraph root files =
    fmap graph $ sequence $ fmap idAndDep files
    where
        idAndDep file =
            getDependencies root file >>= \deps ->
            return (identifier file, deps)

getMap:: FilePath -> FilePath -> IO (Map String (Maybe String))
getMap root path =
    readListFile root path >>=
    return . Map.fromList . Prelude.map (\(k, v) -> (k, Just v))

readListFile :: Read a => FilePath -> FilePath -> IO [a]
readListFile root path =
    readFileWithDefault "[]" (root </> path) >>= return . read

ignorePaths :: FilePath -> [String] -> [FilePath] -> [FilePath]
ignorePaths root specs paths =
    select (not . matchesSpec) canonPaths
    where
        canonSpecs = fmap splitPath specs
        canonPaths = Prelude.map (makeRelative root) paths
        matchesSpec path =
            any (`isInfixOf` splitPath path) canonSpecs

toRepresentation :: [File] -> Map FilePath File
toRepresentation files =
    Map.fromList $ fmap (\f -> (identifier f, f)) files
