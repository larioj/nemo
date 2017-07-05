module Read where

import File
import Directory
import NemoConfig
import System.FilePath.Posix
import Data.Map as Map
import qualified HaskellRead
import Util
import Data.List (isInfixOf)
import Nemo
import System.Directory (doesFileExist)
import Data.Maybe (catMaybes)
import Graph
import NemoGraph
import Control.Monad (forM)

getNemo :: FilePath -> IO (Nemo String File)
getNemo projectRoot =
    getAllFiles projectRoot >>= \files ->
    getCloneGraph projectRoot >>= \clones ->
    getPredecessorGraph projectRoot >>= \preds ->
    let reps = toRepresentation files
        deps = getDependencyGraph reps projectRoot files in
        return $ Nemo (toRepresentation files) $ NemoGraph {
            dependencyGraph = deps,
            cloneGraph = clones,
            predecessorGraph = preds
        }

getAllSources :: FilePath -> IO [FilePath]
getAllSources projectRoot =
    getIgnoreSpec projectRoot >>= \spec ->
    listDirectoryRecursively projectRoot >>= \paths ->
        return $ selectSupportedPaths $ ignorePaths projectRoot spec paths

getAllFiles :: FilePath -> IO [File]
getAllFiles projectRoot =
    fmap concat $
    getModuleRoots projectRoot >>= \moduleRoots ->
    forM moduleRoots $ \moduleRoot ->
        getAllSources (projectRoot </> moduleRoot) >>= \sources ->
        loadAll projectRoot moduleRoot sources

getCloneGraph :: FilePath -> IO (Map String (Maybe String))
getCloneGraph projectRoot = getMap projectRoot cloneFile

getPredecessorGraph :: FilePath -> IO (Map String (Maybe String))
getPredecessorGraph projectRoot = getMap projectRoot predecessorFile

selectSupportedPaths :: [FilePath] -> [FilePath]
selectSupportedPaths = HaskellRead.selectHaskellPaths

selectSupportedFiles :: [File] -> [File]
selectSupportedFiles = HaskellRead.selectHaskellFiles

-- TODO: will multiplex file types
extractDependencies :: File -> [FilePath]
extractDependencies = HaskellRead.extractDependencies

getDependencies :: Map FilePath File -> FilePath -> File -> [FilePath]
getDependencies reps projectRoot file =
    catMaybes $ (flip fmap) (Read.extractDependencies file) $ \dep ->
        if' (Map.member dep reps) (Just dep) Nothing

getDependencyGraph :: Map FilePath File -> FilePath -> [File] -> Graph String
getDependencyGraph reps projectRoot files =
    graph $ fmap idAndDep files
    where
        idAndDep file =
            (identifier file, getDependencies reps projectRoot file)

getIgnoreSpec :: FilePath -> IO [String]
getIgnoreSpec projectRoot =
    readListFile projectRoot ignoreFile

getModuleRoots :: FilePath -> IO [FilePath]
getModuleRoots projectRoot =
    fmap (\r -> if' (r == []) [""] r) roots
    where
        roots = readListFile projectRoot moduleRootsFile

getMap:: FilePath -> FilePath -> IO (Map String (Maybe String))
getMap projectRoot path =
    readListFile projectRoot path >>=
    return . Map.fromList . Prelude.map (\(k, v) -> (k, Just v))

readListFile :: Read a => FilePath -> FilePath -> IO [a]
readListFile projectRoot path =
    readFileWithDefault "[]" (projectRoot </> path) >>= return . read

ignorePaths :: FilePath -> [String] -> [FilePath] -> [FilePath]
ignorePaths projectRoot specs paths =
    select (not . matchesSpec) canonPaths
    where
        canonSpecs = fmap splitPath specs
        canonPaths = Prelude.map (makeRelative projectRoot) paths
        matchesSpec path =
            any (`isInfixOf` splitPath path) canonSpecs

toRepresentation :: [File] -> Map FilePath File
toRepresentation files =
    Map.fromList $ fmap (\f -> (identifier f, f)) files
