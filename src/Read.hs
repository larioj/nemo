module Read where

import           Control.Monad         (forM)
import           Data.List             (isInfixOf)
import           Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Directory
import           File
import           Graph
import           Nemo
import           Config
import           NemoGraph
import           NemoPath
import           System.FilePath.Posix
import           Util
import ReadApi

getNemo :: FilePath -> IO (Nemo String File)
getNemo projectRoot =
    getAllFiles projectRoot >>= \files ->
    getCloneGraph projectRoot >>= \clones ->
    getPredecessorGraph projectRoot >>= \preds ->
    let reps = toRepresentation files
        deps = getDependencyGraph reps files in
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

getDependencyGraph :: Map FilePath File -> [File] -> Graph String
getDependencyGraph reps files =
    graph $ fmap idAndDep files
    where
        idAndDep file =
            (identifier file, getDependencies reps file)

getDependencies :: Map FilePath File -> File -> [FilePath]
getDependencies reps file =
    catMaybes $ (flip fmap) (extractDependencies file) $ \dep ->
        if' (Map.member dep reps) (Just dep) Nothing

toRepresentation :: [File] -> Map FilePath File
toRepresentation files =
    Map.fromList $ fmap (\f -> (identifier f, f)) files
