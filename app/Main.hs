module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.RWS.Lazy (RWST, runRWST)
import           Data.Nemo.Env          (Env, loadEnv)
import           Data.Nemo.Log          (Log)
import           Nemo.Cat               (cat)
import qualified Nemo.CheckIn           as CheckIn
import qualified Nemo.Eval              as Eval
import qualified Nemo.Init              as Init
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure)

main :: IO ()
main = do
  args <- getArgs
  if args == ["init"]
    then Init.init
    else do
      env <- loadEnv
      runRWST (runCommand args) env () >> return ()

runCommand :: [String] -> RWST Env Log () IO ()
runCommand args =
  case args of
    ["copy", path]         -> CheckIn.copy path >> return ()
    ["move", path]         -> CheckIn.move path >> return ()
    ["cat", path]          -> cat path
    ["eval", "copy", path] -> Eval.copy path >> return ()
    ["eval", "move", path] -> Eval.move path >> return ()
    _                      -> liftIO $ exitFailure
