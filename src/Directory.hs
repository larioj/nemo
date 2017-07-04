module Directory where

import Util
import System.Directory
import Control.Monad
import System.FilePath.Posix
import Data.Traversable
import Data.List (isInfixOf)

ignorePaths :: [String] -> [FilePath] -> [FilePath]
ignorePaths specs paths =
    select (not . matchesSpec) paths
    where
        canonSpecs = fmap splitPath specs
        matchesSpec path =
            any (`isInfixOf` splitPath path) canonSpecs

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively root =
    fmap concat $
    listDirectory root >>= \dirContents ->
    forM (map (root </>) dirContents) $ \fso ->
        ifM (doesDirectoryExist fso)
            (listDirectoryRecursively fso)
            (return [fso])

-- Requires: Marker must be relative path to a file
findParentWithMarker :: FilePath -> FilePath -> IO (Maybe FilePath)
findParentWithMarker marker start =
    ifM (doesPathExist (start </> marker))
        (return $ Just start)
        (case getParent start of
            Nothing -> return Nothing
            Just parent -> findParentWithMarker marker parent)

getParent :: FilePath -> Maybe FilePath
getParent "." = Nothing
getParent "/" = Nothing
getParent dir = Just . takeDirectory $ dir
