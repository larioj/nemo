module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import qualified Data.HashTable.IO   as H
import           Data.List.Split
import           Data.Maybe
import           Data.Traversable
import           Lib
import           Prelude             hiding (init)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           Text.Printf         (printf)

nemoDir :: FilePath
nemoDir = "nemolib"

getNemoPath :: IO (Maybe FilePath)
getNemoPath = getMarkerPath nemoDir

fromMaybeOrDie :: IO (Maybe a) -> IO a
fromMaybeOrDie iom = do
  m <- iom
  case m of
    Nothing -> exitFailure
    Just a  -> return a

getNemoPathOrDie :: IO FilePath
getNemoPathOrDie = fromMaybeOrDie getNemoPath

srcDir :: FilePath
srcDir = joinPath [nemoDir, "src"]

getSrcPath :: IO (Maybe FilePath)
getSrcPath = getMarkerPath srcDir

getSrcPathOrDie :: IO FilePath
getSrcPathOrDie = fromMaybeOrDie getSrcPath

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]          -> init
    ["checkin", path] -> checkin path
    ["cat", path]     -> cat path
    other             -> exitFailure

init :: IO ()
init = do
  touchDirectory nemoDir
  touchDirectory srcDir

checkin :: FilePath -> IO ()
checkin source = do
  srcPath <- getSrcPathOrDie
  hash <- fmap alphaHash (readFile source)
  let name = printf "%s_%s" (takeBaseName source) hash
  let dest = joinPath [srcPath, name]
  renameFile source dest
  -- TODO(larioj): make file ro
  putStrLn name

type MutableMap = H.BasicHashTable String String

type MutableSet = H.BasicHashTable String Bool

cat :: FilePath -> IO ()
cat source = do
  seen <- H.new :: IO MutableSet
  append seen source

append :: MutableSet -> FilePath -> IO ()
append seen source = do
  H.insert seen source True
  srcsDir <- getSrcPathOrDie
  translations <- H.new :: IO MutableMap
  content <- readFile source
  for_ (lines content) $ \line ->
    case parseDirective line of
      Export alias ->
        let hash = getHash source
            name = alias ++ "_" ++ hash
         in unless (null hash) $ H.insert translations alias name
      Include name alias -> do
        H.insert translations alias name
        let newSource = srcDir </> name
        alreadyAppended <- H.lookup seen newSource
        unless (fromMaybe False alreadyAppended) $ append seen newSource
      Content tokens -> do
        translated <- translate translations tokens
        putStrLn $ concat translated

translate :: MutableMap -> [String] -> IO [String]
translate translations tokens =
  for tokens $ \token -> do
    name <- H.lookup translations token
    return $ fromMaybe token name

getHash :: FilePath -> String
getHash = fromMaybe "" . (`at` 1) . splitOn "_" . takeBaseName
