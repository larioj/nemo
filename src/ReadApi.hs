module ReadApi where

import           File
import qualified HaskellRead

isSupportedFilePath :: FilePath -> Bool
isSupportedFilePath =
    HaskellRead.isSupportedFilePath

isSupportedFile :: File -> Bool
isSupportedFile =
    HaskellRead.isSupportedFile

extractDependencies :: File -> [FilePath]
extractDependencies =
    HaskellRead.extractDependencies
