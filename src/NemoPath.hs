module NemoPath where

import System.FilePath.Posix
    ( makeRelative
    , (</>)
    , isRelative
    , isAbsolute
    )

data NemoPath =
    NemoPath
        { project      :: FilePath
        , subdirectory :: FilePath
        , filepath     :: FilePath
        } deriving (Show, Eq, Ord)

makeNemoPath :: FilePath -> FilePath -> FilePath -> NemoPath
makeNemoPath project subdirectory filepath
    | isRelative project =
        error $ "NemoPath: project must be absolute"
    | isAbsolute ssub =
        error $ "NemoPath: subdirectory must be relative"
    | isAbsolute sfile =
        error $ "NemoPath: filepath must be relative"
    | isEmpty sfile =
        error $ "NemoPath: filepath must not be empty"
    | otherwise =
        NemoPath project ssub sfile
    where
        ssub = sanitize $ makeRelative project subdirectory
        absoluteSubdirectory = project </> ssub
        sfile = sanitize $ makeRelative absoluteSubdirectory filepath

toFilePath :: NemoPath -> FilePath
toFilePath (NemoPath proj mod file) =
    proj </> mod </> file

sanitize :: FilePath -> FilePath
sanitize "."                = ""
sanitize "./"               = ""
sanitize ('.' : '/' : rest) = rest
sanitize s                  = s

isEmpty :: FilePath -> Bool
isEmpty f = (sanitize f) == ""
