module Directory where

import           Control.Monad
import           System.Directory
import           System.FilePath.Posix
import           Util

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
            Nothing     -> return Nothing
            Just parent -> findParentWithMarker marker parent)

getParent :: FilePath -> Maybe FilePath
getParent "." = Nothing
getParent "/" = Nothing
getParent dir = Just . takeDirectory $ dir
