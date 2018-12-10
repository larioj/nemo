module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.RWS.Lazy (RWST, runRWST)
import           Data.Nemo.Env          (Env, loadEnv)
import qualified Data.Nemo.Env          as Env
import           Data.Nemo.Log          (Log)
import           Nemo.Cat               (cat)
import qualified Nemo.CheckIn           as CheckIn
import qualified Nemo.Eval              as Eval
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  env <- loadEnv
  if args == ["init"]
    then Env.init
    else runRWST (runCommand args) env () >> return ()

runCommand :: [String] -> RWST Env Log () IO ()
runCommand args =
  case args of
    ["copy", path]         -> CheckIn.copy path >> return ()
    ["move", path]         -> CheckIn.move path >> return ()
    ["cat", path]          -> cat path
    ["eval", "copy", path] -> Eval.copy path >> return ()
    ["eval", "move", path] -> Eval.move path >> return ()
    _                      -> liftIO $ exitFailure
