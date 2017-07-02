module Haskell where

import Nemo
import File
import EscapeRegex
import NemoGraph
import Text.Regex.Posix
import Data.List.Utils (replace)
import Data.Set as Set
import Data.Map as Map
import Hash

-- TODO: This module could be better

-- TODO: assumes FilePath separator is /
-- Requires: File must be relative to root of project
fileToModule :: File -> String
fileToModule file =
    replace "/" "." $ (name file) ++ (directory file)

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
            replace oldImport newImport contents
            where newImport = replace old new oldImport

replaceModuleDeclaration :: String -> String -> String -> String
replaceModuleDeclaration old new contents =
    replace oldMod newMod contents
    where
        oldMod = extractModuleDeclaration old contents
        newMod = replace old new oldMod

extractImports :: String -> String -> [String]
extractImports mod contents =
    getAllTextMatches $ contents =~ (importRegex mod)

extractModuleDeclaration :: String -> String -> String
extractModuleDeclaration mod contents =
    contents =~ moduleRegex mod

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
makeClone nemo@(Nemo rep g) original = (identifier newFile, newFile)
    where
        originalFile = rep Map.! original
        newFile = replaceName $ replaceDependencies nemo originalFile
