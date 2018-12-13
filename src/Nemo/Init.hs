module Nemo.Init where

import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Nemo.Env          (makeEnv, metadata, nemo, sources)
import qualified Nemo.Config            as Config
import           System.Nemo            (createDirectoryIfNotExists)

init :: MonadIO m => m ()
init = do
  liftIO $ createDirectoryIfNotExists (env ^. nemo)
  liftIO $ createDirectoryIfNotExists (env ^. sources)
  liftIO $ createDirectoryIfNotExists (env ^. metadata)
  where
    env = makeEnv Config.libName
