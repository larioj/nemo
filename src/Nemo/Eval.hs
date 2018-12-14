module Nemo.Eval where

import           Control.Lens                (re, (^.))
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.RWS.Lazy      (RWST)
import           Data.Foldable               (for_)
import           Data.Nemo.Env               (Env)
import           Data.Nemo.Eval.Expression   (Expression (Copy, Move), target)
import qualified Data.Nemo.Eval.Expression   as Exp
import           Data.Nemo.Log               (Log)
import           Data.Nemo.Name              (Name)
import           Data.Nemo.NcuInfo           (name)
import qualified Nemo.CheckIn                as CheckIn
import           Parser.Nemo.Directive       (_Directive)
import qualified Parser.Nemo.Eval.Expression as ParseExp
import           System.FilePath             (joinPath)
import           System.FilePath.Lens        (basename, filename)
import           System.IO                   (IOMode (WriteMode), hClose,
                                              hPutStrLn, openFile)
import           System.IO.Temp              (createTempDirectory)

copy :: FilePath -> RWST Env Log a IO Name
copy path = eval $ Copy path (path ^. basename)

move :: FilePath -> RWST Env Log a IO Name
move path = eval $ Move path (path ^. basename)

eval :: Expression -> RWST Env Log a IO Name
eval parent = do
  contents <- liftIO $ readFile (parent ^. target)
  tempDir <- liftIO $ createTempDirectory "/tmp" "nemo"
  let tmpPath = joinPath [tempDir, parent ^. target . filename]
  h <- liftIO $ openFile tmpPath WriteMode
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
