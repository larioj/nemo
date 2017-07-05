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
        , moduleRoot :: FilePath
        , projectRoot :: FilePath
        , contents :: String
        } deriving (Eq, Show, Ord)

file :: String
     -> String
     -> FilePath
     -> FilePath
     -> FilePath
     -> String
     -> File
file name ext dir mod proj cont =
    File name ext (san dir) (san mod) (san proj) cont
    where
        san "." = ""
        san "./" = ""
        san s = s

splitFilePath :: FilePath -> (FilePath, String, String)
splitFilePath p = (dir, name, ext)
    where
        (dir, fileName) = splitFileName p
        (name, ext) = splitExtension fileName

filePath :: File -> FilePath
filePath (File name ext dir mod proj _) =
    proj </> mod </> dir </> (name ++ ext)

load :: FilePath -> FilePath -> FilePath -> IO File
load proj mod p =
    fmap (file name ext dir mod proj) (readFile abs)
    where
        abs = proj </> mod </> p
        (dir, name, ext) = splitFilePath $ makeRelative (proj </> mod) p

loadAll :: FilePath -> FilePath -> [FilePath] -> IO [File]
loadAll proj mod paths =
    sequence $ fmap (load proj mod) paths

dump :: File -> IO ()
dump f = writeFile (filePath f) (contents f)

identifier :: File -> String
identifier file =
    (directory file) </>
    ((name file) ++ (extension file))

readFileWithDefault :: String -> FilePath -> IO String
readFileWithDefault defCont path =
    ifM (doesPathExist path)
        (readFile path)
        (return defCont)

