module File where

import           NemoPath
import           System.Directory
import           System.FilePath.Posix
import           Util

data File =
    File
        { path     :: NemoPath
        , contents :: String
        } deriving (Eq, Show, Ord)

makeFile :: NemoPath -> String -> File
makeFile path contents = File path contents

load :: NemoPath -> IO File
load path =
    fmap (makeFile path) (readFile $ toFilePath path)

loadAll :: [NemoPath] -> IO [File]
loadAll paths =
    sequence $ fmap load paths

dump :: File -> IO ()
dump file =
    ifM (doesFileExist filePath)
        (return ()) $
        writeFile filePath (contents file)
    where
        filePath = toFilePath $ path file

dumpAll :: [File] -> IO ()
dumpAll files =
    mapM_ dump files

identifier :: File -> String
identifier file = filePart file

extension :: File -> String
extension file =
    takeExtension $ filePart file

filePart :: File -> FilePath
filePart file = filepath $ path file

replaceFilePart :: FilePath -> File -> File
replaceFilePart new file =
    makeFile newPath (contents file)
    where
        oldPath = path file
        proj = project oldPath
        sub = subdirectory oldPath
        newPath = makeNemoPath proj sub new

replaceContents :: String -> File -> File
replaceContents new file =
    makeFile (path file) new

replaceSubdirectoryPart :: FilePath -> File -> File
replaceSubdirectoryPart new file =
    makeFile newPath (contents file)
    where
        oldPath = path file
        proj = project oldPath
        filePart = filepath oldPath
        newPath = makeNemoPath proj new filePart
