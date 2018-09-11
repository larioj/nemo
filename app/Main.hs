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
import           System.IO
import           Text.Printf         (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]          -> init
    ["checkin", path] -> putStrLn =<< checkin Nothing path
    ["cat", path]     -> cat path
    other             -> exitFailure

init :: IO ()
init = do
  touchDirectory nemoDir
  touchDirectory srcDir

checkin :: Maybe (H.BasicHashTable String String) -> FilePath -> IO String
checkin maybeDone source = do
  done <-
    case maybeDone of
      Just d  -> return d
      Nothing -> H.new
  srcPath <- getSrcPathOrDie
  (tmpPath, h) <- openTempFile "/tmp" "nemo"
  content <- readFile source
  for_ (lines content) $ \line -> do
    oldDirective <- parseDirectiveOrDie line
    directive <-
      case oldDirective of
        Include (Right (Checkin path)) alias -> do
          maybeName <- H.lookup done path
          name <-
            case maybeName of
              Just n  -> return n
              Nothing -> checkin (Just done) path
          return $ Include (Left name) alias
        other -> return other
    hPutStrLn h (showDirective directive)
  hClose h
  hash <- fmap alphaHash (readFile tmpPath)
  let name = concat [takeBaseName source, "_", hash]
  renameFile tmpPath $ joinPath [srcPath, name]
  -- TODO(larioj): make file ro
  H.insert done source name
  return name

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
  for_ (lines content) $ \line -> do
    directive <- parseDirectiveOrDie line
    case directive of
      Export alias ->
        let hash = getHash source
            name = alias ++ "_" ++ hash
         in unless (null hash) $ H.insert translations alias name
      Include e alias -> do
        name <-
          case e of
            Left n               -> return n
            Right (Checkin path) -> checkin Nothing path
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
