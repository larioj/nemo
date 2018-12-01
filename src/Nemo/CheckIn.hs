module Nemo.CheckIn where

import           Control.Lens            (over, view, (^.), (^?))
import           Control.Monad           (unless)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.RWS.Lazy  (RWST, gets, modify)
import           Control.Nemo            (runState)
import           Crypto.Hash.SHA256      (finalize, init, update)
import           Data.ByteString.Char8   (pack, unpack)
import           Data.Foldable           (for_)
import           Data.List.Stack         (push)
import           Data.Nemo.CheckIn.State (State (State), exports, hashCtx)
import           Data.Nemo.Directive     (Directive (Export))
import           Data.Nemo.Directive     (_Directive)
import           Data.Nemo.Env           (Env)
import           Data.Nemo.Error         (maybeDie)
import qualified Data.Nemo.Error         as Err
import           Data.Nemo.Log           (Log)
import           Data.Nemo.Name          (Name (Name))
import           Data.Nemo.NcuInfo       (NcuInfo, canonicalNcuInfo,
                                          contentPath, name, updateName,
                                          writeNcuInfo)
import           Nemo.Hash               (encode)
import           Prelude                 hiding (init)
import           System.Directory        (copyFile, renameFile)
import           System.Nemo             (makeReadOnly)
import           System.Posix.Files      (createSymbolicLink)

move :: FilePath -> RWST Env Log a IO NcuInfo
move = runCheckIn renameFile

copy :: FilePath -> RWST Env Log a IO NcuInfo
copy = runCheckIn copyFile

runCheckIn ::
     (FilePath -> FilePath -> IO ()) -> FilePath -> RWST Env Log a IO NcuInfo
runCheckIn opOnFile = runState (State init []) . checkIn opOnFile

checkIn ::
     (FilePath -> FilePath -> IO ())
  -> FilePath
  -> RWST Env Log State IO NcuInfo
checkIn opOnFile path = do
  contents <- liftIO $ readFile path
  for_ (lines contents) $ \line -> do
    directive <- maybeDie Err.BadDirective $ line ^? _Directive
    case directive of
      Export _ prefix -> modify $ over exports (push prefix)
      _               -> return ()
    modify $ over hashCtx (flip update (pack line))
  hash <- gets $ encode . finalize . view hashCtx
  names <- gets $ fmap (flip Name hash) . view exports
  canonInfo <- canonicalNcuInfo path hash
  liftIO $ opOnFile path (canonInfo ^. contentPath)
  makeReadOnly (canonInfo ^. contentPath)
  writeNcuInfo canonInfo
  for_ names $ \curName ->
    unless (curName == canonInfo ^. name) $ do
      let info = updateName canonInfo curName
      liftIO $
        createSymbolicLink (canonInfo ^. contentPath) (info ^. contentPath)
      writeNcuInfo info
  return canonInfo
