module Nemo.Eval where

import           Control.Lens              (re, (^.))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.RWS.Lazy    (RWST)
import           Data.Foldable             (for_)
import           Data.Nemo.Directive       (_Directive)
import           Data.Nemo.Env             (Env)
import           Data.Nemo.Eval.Expression (Expression (Copy, Move), target)
import qualified Data.Nemo.Eval.Expression as Exp
import           Data.Nemo.Log             (Log)
import           Data.Nemo.Name            (Name)
import           Data.Nemo.NcuInfo         (name)
import           Nemo.CheckIn              (copy, move)
import           System.IO                 (hClose, hPutStrLn, openTempFile)

eval :: Expression -> RWST Env Log a IO Name
eval parent = do
  contents <- liftIO $ readFile (parent ^. target)
  (tmpPath, h) <- liftIO $ openTempFile "/tmp" "nemo"
  for_ (lines contents) $ \line -> do
    lout <-
      case Exp.fromString line of
        Just child -> do
          childName <- eval child
          return $ Exp.toDirective childName child ^. re _Directive
        Nothing -> return line
    liftIO $ hPutStrLn h lout
  liftIO $ hClose h
  info <-
    case parent of
      Copy _ _ -> copy tmpPath
      Move _ _ -> move tmpPath
  return $ info ^. name
