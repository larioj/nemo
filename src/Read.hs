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


getNemo :: FilePath -> Nemo String File
getNemo = undefined

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

getClones :: FilePath -> IO (Map String (Maybe String))
getClones root = getMap root cloneFile

getPredecessors :: FilePath -> IO (Map String (Maybe String))
getPredecessors root = getMap root predecessorFile

selectSupportedPaths :: [FilePath] -> [FilePath]
selectSupportedPaths = selectHaskellPaths

selectSupportedFiles :: [File] -> [File]
selectSupportedFiles = selectHaskellFiles

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
