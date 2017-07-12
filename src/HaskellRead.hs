module HaskellRead where

import           File
import           System.FilePath.Posix (takeExtension)
import           Text.Regex.Posix
import           Util

isSupportedFilePath :: FilePath -> Bool
isSupportedFilePath = (==) ".hs" . takeExtension

isSupportedFile :: File -> Bool
isSupportedFile file =
    (extension file) == ".hs"

extractDependencies :: File -> [String]
extractDependencies file =
    Prelude.map (moduleToFilePath . captureModule) imports
    where
        imports = extractImportExpressions file

moduleToFilePath :: String -> FilePath
moduleToFilePath mod =
    replaceSafe "." "/" mod ++ ".hs"

extractImportExpressions :: File -> [String]
extractImportExpressions file =
    getAllTextMatches $ (contents file) =~ importRegex

importRegex :: String
importRegex = "import([\r\n\t\f\v ]+qualified)?[\r\n\t\f\v ]+([^\r\n\t\f\v ]+)"

captureModule :: String -> String
captureModule importLine = mod
    where
        match = importLine =~ importRegex :: (String, String, String, [String])
        (_, _, _, captures) = match
        mod = captures !! 1
