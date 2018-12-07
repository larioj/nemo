{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Env where

import           Control.Lens           (makeLenses, (^.))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Nemo.Error        (maybeDie)
import qualified Data.Nemo.Error        as Err
import qualified Nemo.Config            as Config
import           Nemo.Util              (findMarker)
import           System.FilePath        (takeDirectory, (</>))
import           System.Nemo            (touchDirectory)

data Env = Env
  { _base     :: FilePath
  , _nemo     :: FilePath
  , _sources  :: FilePath
  , _metadata :: FilePath
  } deriving (Show, Eq, Ord)

makeLenses ''Env

loadEnv :: MonadIO m => String -> m Env
loadEnv marker = do
  fmap env . maybeDie Err.UnableToFindMarker =<< liftIO (findMarker marker)

env :: String -> Env
env nemo =
  Env
    { _base = takeDirectory nemo
    , _nemo = nemo
    , _sources = nemo </> Config.srcName
    , _metadata = nemo </> Config.metadataName
    }

init :: MonadIO m => m ()
init = do
  liftIO $ touchDirectory (e ^. nemo)
  liftIO $ touchDirectory (e ^. sources)
  liftIO $ touchDirectory (e ^. metadata)
  where
    e = env Config.libName
