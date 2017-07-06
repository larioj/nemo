module Main where

import System.Directory
import System.Environment (getArgs)
import Directory
import System.FilePath.Posix
import File
import Util
import HaskellRead
import Read
import Nemo
import NemoGraph
import NemoConfig
import Update
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (catMaybes)

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
    createDirectoryIfMissing True configDir

synch :: FilePath -> IO ()
synch projectRoot = putStrLn "fatal: Not implemented"

status :: FilePath -> IO ()
status projectRoot =
    Read.getNemo projectRoot >>= \old ->
    showNewFiles old (Update.update old)

showNewFiles :: Nemo FilePath File -> Nemo FilePath File -> IO ()
showNewFiles old new =
    prettyPrintList clean
    where
        getFiles = Map.toList . cloneGraph . nemoGraph
        oldFiles = getFiles old
        newFiles = getFiles new
        diff = newFiles List.\\ oldFiles
        wrap (a, mb) = fmap (\b -> (a, b)) mb
        clean = catMaybes $ fmap wrap diff

withPreconditions :: (FilePath -> IO ()) -> IO ()
withPreconditions inner =
    getCurrentDirectory >>= \cwd ->
    findParentWithMarker configDir cwd >>= \projectRoot ->
    case projectRoot of
        Nothing -> putStrLn "fatal: Not a nemo project"
        Just projectRoot -> inner projectRoot
