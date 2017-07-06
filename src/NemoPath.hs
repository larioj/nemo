module NemoPath where

import System.FilePath.Posix ((</>), makeRelative)

data NemoPath =
    NemoPath
        { projectPart :: FilePath
        , modulePart  :: FilePath
        , filePart    :: FilePath
        } deriving (Show, Eq, Ord)

-- TODO: add more checking, i.e. check tha proj is absolute
-- TODO: and that file is not empty
nemoPath :: FilePath -> FilePath -> FilePath -> NemoPath
nemoPath proj mod file =
    NemoPath proj smod (makeRelative (proj </> smod) file)
    where
        san "." = ""
        san "./" = ""
        san s = s
        smod = san mod

toFilePath :: NemoPath -> FilePath
toFilePath (NemoPath proj mod file) =
    proj </> mod </> file
