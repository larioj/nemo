module Main where

import Lib
import System.Exit
import System.FilePath
import System.Environment
import Prelude hiding (init)

nemoPath = "nemo"
srcsPath = joinPath [nemoPath, "src"]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init"] -> init
    other -> exitFailure

init :: IO ()
init = do
  touchDirectory nemoPath
  touchDirectory srcsPath

  
