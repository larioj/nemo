module File where

import System.FilePath.Posix
    ( splitFileName
    , splitExtension
    , (</>)
    )

-- example
-- file: /foo/bar.txt
-- contents: hello world
-- File { "bar", ".txt", "/foo/", "hello world" }
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

load :: FilePath -> IO File
load p = fmap (File name ext dir) (readFile p)
    where
        (name, ext, dir) = splitFilePath p

dump :: File -> IO ()
dump f = writeFile (filePath f) (contents f)
