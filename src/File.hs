module File where

import System.FilePath.Posix
    ( splitFileName
    , splitExtension
    , (</>)
    , makeRelative
    )
import System.Directory
import Util

{- Example
     file: foo/bar.txt
     contents: hello world
       ->
     File { "bar", ".txt", "/foo/", "hello world" }

   Note: directories are always relative to the root of the
   project.
-}
data File =
    File
        { name :: String
        , extension :: String
        , directory :: FilePath
        , contents :: String
        } deriving (Eq, Show, Ord)

splitFilePath :: FilePath -> (FilePath, String, String)
splitFilePath p = (dir, name, ext)
    where
        (dir, fileName) = splitFileName p
        (name, ext) = splitExtension fileName

filePath :: File -> FilePath
filePath (File name ext dir _) = dir </> (name ++ ext)

load :: FilePath -> FilePath -> IO File
load root p =
    fmap (File name ext dir) (readFile p)
    where
        (name, ext, dir) = splitFilePath $ makeRelative root p

loadAll :: FilePath -> [FilePath] -> IO [File]
loadAll root paths =
    sequence $ fmap (load root) paths

dump :: FilePath -> File -> IO ()
dump root f = writeFile (root </> filePath f) (contents f)

identifier :: File -> String
identifier file =
    (directory file) ++ (name file) ++ (extension file)

readFileWithDefault :: String -> FilePath -> IO String
readFileWithDefault defCont path =
    ifM (doesPathExist path)
        (readFile path)
        (return defCont)

