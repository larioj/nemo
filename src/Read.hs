module Read where

import           Control.Monad         (forM)
import           Data.List             (isInfixOf)
import           Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Directory
import           File
import           Graph
import qualified HaskellRead
import           Nemo
import           NemoConfig
import           NemoGraph
import           NemoPath
import           System.Directory      (doesFileExist)
import           System.FilePath.Posix
import           Util

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

getAllFiles :: FilePath -> IO [File]
getAllFiles projectRoot =
    getSelectedSources projectRoot >>= loadAll

getSelectedSources :: FilePath -> IO [NemoPath]
getSelectedSources projectRoot =
    getIgnoreSpec projectRoot >>= \spec ->
    getAllSources projectRoot >>= \sources ->
        return $ selectSources spec sources

getAllSources :: FilePath -> IO [NemoPath]
getAllSources projectRoot =
    fmap concat $
    getModuleRoots projectRoot >>= \moduleRoots ->
    forM moduleRoots $ \moduleRoot ->
        getModuleRootSources projectRoot moduleRoot

getModuleRootSources :: FilePath -> FilePath -> IO [NemoPath]
getModuleRootSources projectRoot moduleRoot =
    listDirectoryRecursively (projectRoot </> moduleRoot) >>= \paths ->
        return $ fmap (makeNemoPath projectRoot moduleRoot) paths

selectSources :: [FilePath] -> [NemoPath] -> [NemoPath]
selectSources specs sources =
    removeIgnoredNemoPaths specs $
    selectSupportedNemoPaths sources

selectSupportedNemoPaths :: [NemoPath] -> [NemoPath]
selectSupportedNemoPaths paths =
    select (isSupportedFilePath . toFilePath) paths

isSupportedFilePath :: FilePath -> Bool
isSupportedFilePath = HaskellRead.isHaskellFilePath

removeIgnoredNemoPaths :: [FilePath] -> [NemoPath] -> [NemoPath]
removeIgnoredNemoPaths spec paths =
    select (not . isIgnoredNemoPath spec) paths

-- Requires: specs are relative to project root
isIgnoredNemoPath :: [FilePath] -> NemoPath -> Bool
isIgnoredNemoPath specs np =
    any (`isInfixOf` canonPath) canonSpecs
    where
        canonSpecs = fmap splitPath specs
        canonPath = splitPath $ (subdirectory np) </> (filepath np)

getCloneGraph :: FilePath -> IO (Map String (Maybe String))
getCloneGraph projectRoot = getMap projectRoot cloneFile

getPredecessorGraph :: FilePath -> IO (Map String (Maybe String))
getPredecessorGraph projectRoot = getMap projectRoot predecessorFile

-- TODO: will multiplex file types
extractDependencies :: File -> [FilePath]
extractDependencies = HaskellRead.extractDependencies

getDependencyGraph :: Map FilePath File -> FilePath -> [File] -> Graph String
getDependencyGraph reps projectRoot files =
    graph $ fmap idAndDep files
    where
        idAndDep file =
            (identifier file, getDependencies reps projectRoot file)

getDependencies :: Map FilePath File -> FilePath -> File -> [FilePath]
getDependencies reps projectRoot file =
    catMaybes $ (flip fmap) (Read.extractDependencies file) $ \dep ->
        if' (Map.member dep reps) (Just dep) Nothing

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

toRepresentation :: [File] -> Map FilePath File
toRepresentation files =
    Map.fromList $ fmap (\f -> (identifier f, f)) files
