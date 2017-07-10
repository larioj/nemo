module HaskellTransform where

import Nemo
import File
import EscapeRegex
import NemoGraph
import Text.Regex.Posix
import Data.Set as Set
import Data.Map as Map
import Hash
import System.FilePath.Posix
import Data.Maybe (fromMaybe)
import Util
import HaskellRead ( moduleToFilePath )

makeClone :: Nemo String File -> String -> (String, File)
makeClone nemo@(Nemo rep _) original =
    (identifier newFile, newFile)
    where
        originalFile = rep Map.! original
        newFile =
            replaceModuleWithHash $
            replaceDependencies nemo originalFile

replaceModuleWithHash :: File -> File
replaceModuleWithHash file =
    replaceModule new file
    where
        old = filePathToModule $ filePart file
        hash = base16AlphaHash $ contents file
        new = old ++ "_" ++ hash

replaceModule :: String -> File -> File
replaceModule new file =
    replaceFilePart (moduleToFilePath new) $
    replaceModuleDeclaration new file

replaceModuleDeclaration :: String -> File -> File
replaceModuleDeclaration new file =
    replaceContents newContents file
    where
        old = filePathToModule $ filePart file
        oldDec = extractModuleDeclaration old (contents file)
        newDec = fmap (replaceSafe old new) oldDec
        newContents =
            fromMaybe (contents file) $
            oldDec >>= \oldDec ->
            newDec >>= \newDec ->
                return $ replaceSafe oldDec newDec (contents file)

extractModuleDeclaration :: String -> String -> Maybe String
extractModuleDeclaration mod contents =
    contents =~~ moduleRegex mod

replaceDependencies :: Nemo String File -> File -> File
replaceDependencies (Nemo _ g) file =
    Prelude.foldl replaceDependency' file (zip oldDeps newDeps)
    where
        oldDeps = Set.toList $ dependencies g (identifier file)
        newDeps = fmap (cloneOrSelf g) oldDeps
        replaceDependency' :: File -> (FilePath, FilePath) -> File
        replaceDependency' file (old, new) =
            replaceDependency old new file
    
replaceDependency :: FilePath -> FilePath -> File -> File
replaceDependency old new file =
    replaceModuleImports oldMod newMod file
    where
        oldMod = filePathToModule old
        newMod = filePathToModule new

replaceModuleImports :: String -> String -> File -> File
replaceModuleImports old new file =
    Prelude.foldl replaceModuleImportDeclaration file (zip oldDecs newDecs)
    where
        oldDecs = extractModuleImportDeclarations old (contents file)
        newDecs = fmap (replaceSafe old new) oldDecs

replaceModuleImportDeclaration :: File -> (String, String) -> File
replaceModuleImportDeclaration file (old, new) =
    replaceContents newContents file
    where
        newContents = replaceSafe old new (contents file)

extractModuleImportDeclarations :: String -> String -> [String]
extractModuleImportDeclarations mod contents =
    getAllTextMatches $ contents =~ (importRegex mod)

filePathToModule :: FilePath -> String
filePathToModule path =
    replaceSafe "/" "." $ takeExtension path

importRegex :: String -> String
importRegex mod =
    "import([\r\n\t\f\v ]+qualified)?[\r\n\t\f\v ]+" ++
     (escapeRegex mod)

moduleRegex :: String -> String
moduleRegex mod = "module[\r\n\t\f\v ]+" ++ mod



