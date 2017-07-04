module Main where

import System.Directory
import System.Environment (getArgs)
import Directory
import System.FilePath.Posix
import File
import Util

configLocation = "NemoLib"
ignoreLocation = "NemoLib/ignore"
cloneLocation = "NemoLib/clones"
predecessorLocation = "NemoLib/predecessors"

main :: IO ()
main = getArgs >>= nemo

nemo :: [String] -> IO ()
nemo ("synch" : []) = withPreconditions synch
nemo ("init" : []) = Main.init
nemo ("status": []) = withPreconditions status
nemo args = usage args

usage :: [String] -> IO ()
usage args = putStrLn $ "unrecognized arguments: " ++ show args

init :: IO ()
init =
    createDirectoryIfMissing True "NemoLib"

synch :: FilePath -> IO ()
synch root = putStrLn "fatal: Not implemented"

status :: FilePath -> IO ()
status root =
    getAllSources root >>= prettyPrintList

withPreconditions :: (FilePath -> IO ()) -> IO ()
withPreconditions inner =
    getCurrentDirectory >>= \cwd ->
    findParentWithMarker configLocation cwd >>= \root ->
    case root of
        Nothing -> putStrLn "fatal: Not a nemo project"
        Just root -> inner root


getIgnoreSpec :: FilePath -> IO [String]
getIgnoreSpec root =
    readFileWithDefault "[]" (root </> ignoreLocation) >>=
    return . read

getAllSources :: FilePath -> IO [FilePath]
getAllSources root =
    getIgnoreSpec root >>= \spec ->
    listDirectoryRecursively root >>= \paths ->
        return $ ignorePaths spec paths


