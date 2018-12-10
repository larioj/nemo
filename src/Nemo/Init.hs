module Nemo.Init where

import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Nemo.Env          (makeEnv, metadata, nemo, sources)
import qualified Nemo.Config            as Config
import           System.Nemo            (touchDirectory)

init :: MonadIO m => m ()
init = do
  liftIO $ touchDirectory (env ^. nemo)
  liftIO $ touchDirectory (env ^. sources)
  liftIO $ touchDirectory (env ^. metadata)
  where
    env = makeEnv Config.libName
