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
import Control.Monad (forM)

getNemo :: FilePath -> IO (Nemo String File)
getNemo projectRoot =
    getAllFiles projectRoot >>= \files ->
    getDependencyGraph projectRoot files >>= \deps ->
    getCloneGraph projectRoot >>= \clones ->
    getPredecessorGraph projectRoot >>= \preds ->
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
        getAllSources (projectRoot </> moduleRoot) >>=
        loadAll projectRoot moduleRoot

getCloneGraph :: FilePath -> IO (Map String (Maybe String))
getCloneGraph projectRoot = getMap projectRoot cloneFile

getPredecessorGraph :: FilePath -> IO (Map String (Maybe String))
getPredecessorGraph projectRoot = getMap projectRoot predecessorFile

selectSupportedPaths :: [FilePath] -> [FilePath]
selectSupportedPaths = selectHaskellPaths

selectSupportedFiles :: [File] -> [File]
selectSupportedFiles = selectHaskellFiles

-- TODO: will multiplex file types
extractDependencies :: File -> [FilePath]
extractDependencies = HaskellRead.extractDependencies

getDependencies :: FilePath -> File -> IO [FilePath]
getDependencies projectRoot file =
    fmap catMaybes $ sequence $
    (flip fmap) (Read.extractDependencies file) $ \dep ->
        ifM (doesFileExist $ projectRoot </> dep)
            (return $ Just dep)
            (return Nothing)

getDependencyGraph :: FilePath -> [File] -> IO (Graph String)
getDependencyGraph projectRoot files =
    fmap graph $ sequence $ fmap idAndDep files
    where
        idAndDep file =
            getDependencies projectRoot file >>= \deps ->
            return (identifier file, deps)

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
