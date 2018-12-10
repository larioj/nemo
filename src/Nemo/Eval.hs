module Nemo.Eval where

import           Control.Lens                     (re, (^.))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.RWS.Lazy           (RWST)
import           Data.Foldable                    (for_)
import           Data.Nemo.Directive.Parser       (_Directive)
import           Data.Nemo.Env                    (Env)
import           Data.Nemo.Eval.Expression        (Expression (Copy, Move),
                                                   target)
import qualified Data.Nemo.Eval.Expression        as Exp
import qualified Data.Nemo.Eval.Expression.Parser as ParseExp
import           Data.Nemo.Log                    (Log)
import           Data.Nemo.Name                   (Name)
import           Data.Nemo.NcuInfo                (name)
import qualified Nemo.CheckIn                     as CheckIn
import           System.FilePath.Lens             (basename)
import           System.IO                        (hClose, hPutStrLn,
                                                   openTempFile)

copy :: FilePath -> RWST Env Log a IO Name
copy path = eval $ Copy path (path ^. basename)

move :: FilePath -> RWST Env Log a IO Name
move path = eval $ Move path (path ^. basename)

eval :: Expression -> RWST Env Log a IO Name
eval parent = do
  contents <- liftIO $ readFile (parent ^. target)
  (tmpPath, h) <- liftIO $ openTempFile "/tmp" "nemo"
  for_ (lines contents) $ \line -> do
    lout <-
      case ParseExp.fromString line of
        Just child -> do
          childName <- eval child
          return $ Exp.toDirective childName child ^. re _Directive
        Nothing -> return line
    liftIO $ hPutStrLn h lout
  liftIO $ hClose h
  info <-
    case parent of
      Copy _ _ -> CheckIn.copy tmpPath
      Move _ _ -> CheckIn.move tmpPath
  return $ info ^. name
