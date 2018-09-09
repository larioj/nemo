module Main where

import           Control.Applicative
import           Data.Maybe
import           Lib
import           Prelude             hiding (init)
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           Text.Printf         (printf)

nemoDir = "nemo"

srcsDir = joinPath [nemoDir, "src"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"]          -> init
    ["checkin", path] -> checkin path
    other             -> exitFailure

init :: IO ()
init = do
  touchDirectory nemoDir
  touchDirectory srcsDir

checkin :: FilePath -> IO ()
checkin source = do
  nemoPath <- fromMaybe <$> exitFailure <*> getMarkerPath nemoDir
  hash <- fmap alphaHash (readFile source)
  let name = printf "%s_%s" (takeBaseName source) hash
  let dest = joinPath [nemoPath, srcsDir, name]
  renameFile source dest
  -- TODO(larioj): make file ro
  putStrLn name
