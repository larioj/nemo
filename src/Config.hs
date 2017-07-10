module Config where

import           Data.Map              as Map
import           System.FilePath.Posix
import           Util

configDir :: String
configDir = "NemoLib"

ignoreFile :: String
ignoreFile = "NemoLib/ignore"

moduleRootsFile :: String
moduleRootsFile = "NemoLib/moduleRoots"

cloneFile :: String
cloneFile = "NemoLib/clones"

predecessorFile :: String
predecessorFile = "NemoLib/predecessors"

getCloneGraph :: FilePath -> IO (Map String (Maybe String))
getCloneGraph projectRoot = getMap projectRoot cloneFile

getPredecessorGraph :: FilePath -> IO (Map String (Maybe String))
getPredecessorGraph projectRoot = getMap projectRoot predecessorFile

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
