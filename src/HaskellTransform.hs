module HaskellTransform where

import Nemo
import File
import EscapeRegex
import NemoGraph
import Text.Regex.Posix
import Data.Set as Set
import Data.Map as Map
import Hash
import System.FilePath.Posix ((</>))
import Data.Maybe (fromMaybe)
import Util

-- TODO: This module could be better

-- TODO: assumes FilePath separator is /
-- Requires: File must be relative to root of project
fileToModule :: File -> String
fileToModule file =
    replaceSafe "/" "." $ (directory file) </> (name file)

importRegex :: String -> String
importRegex mod =
    "import([\r\n\t\f\v ]+qualified)?[\r\n\t\f\v ]+" ++
     (escapeRegex mod)

moduleRegex :: String -> String
moduleRegex mod = "module[\r\n\t\f\v ]+" ++ mod

replaceImport :: String -> String -> String -> String
replaceImport old new contents =
    Prelude.foldl substituteImport contents imports
    where
        imports = extractImports old contents
        substituteImport contents oldImport =
            replaceSafe oldImport newImport contents
            where newImport = replaceSafe old new oldImport

replaceModuleDeclaration :: String -> String -> String -> String
replaceModuleDeclaration old new contents =
    fromMaybe contents $ -- TODO: add precheck that discards file that are not well formed
    oldMod >>= \oldMod ->
    newMod >>= \newMod ->
        return $ replaceSafe oldMod newMod contents
    where
        oldMod = extractModuleDeclaration old contents
        newMod = fmap (replaceSafe old new) oldMod

extractImports :: String -> String -> [String]
extractImports mod contents =
    getAllTextMatches $ contents =~ (importRegex mod)

extractModuleDeclaration :: String -> String -> Maybe String
extractModuleDeclaration mod contents =
    contents =~~ moduleRegex mod

replaceDependency' :: File -> File -> File -> File
replaceDependency' old new file =
    file { contents = newContents }
    where
        oldModule = fileToModule old
        newModule = fileToModule new
        newContents = replaceImport oldModule newModule (contents file)

replaceDependency :: Nemo String File -> File -> File -> File
replaceDependency (Nemo rep g) old file =
    replaceDependency' old (rep Map.! (cloneOrSelf g (identifier old))) file

replaceDependencies :: Nemo String File -> File -> File
replaceDependencies nemo@(Nemo rep g) file =
    Prelude.foldl (flip $ replaceDependency nemo) file deps
    where
        deps = Set.map (rep Map.!) $ dependencies g (identifier file)

-- Requires: imports have been replaced
replaceName :: File -> File
replaceName file =
    file { name = newName, contents = newContents }
    where
        newName = (name file) ++ "_" ++ (base16AlphaHash (contents file))
        oldModule = fileToModule file
        newModule = fileToModule file { name = newName }
        newContents = replaceModuleDeclaration oldModule newModule (contents file)

makeClone :: Nemo String File -> String -> (String, File)
makeClone nemo@(Nemo rep _) original = (identifier newFile, newFile)
    where
        originalFile = rep Map.! original
        newFile = replaceName $ replaceDependencies nemo originalFile
