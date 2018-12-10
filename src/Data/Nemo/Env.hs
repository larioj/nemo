{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Env where

import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Nemo.Error        (maybeDie)
import qualified Data.Nemo.Error        as Err
import qualified Nemo.Config            as Config
import           Nemo.Util              (findMarker)
import           System.FilePath        (takeDirectory, (</>))

data Env = Env
  { _base     :: FilePath
  , _nemo     :: FilePath
  , _sources  :: FilePath
  , _metadata :: FilePath
  } deriving (Show, Eq, Ord)

makeLenses ''Env

loadEnv :: MonadIO m => m Env
loadEnv =
  fmap makeEnv . maybeDie Err.UnableToFindMarker =<<
  liftIO (findMarker Config.libName)

makeEnv :: String -> Env
makeEnv nemo =
  Env
    { _base = takeDirectory nemo
    , _nemo = nemo
    , _sources = nemo </> Config.srcName
    , _metadata = nemo </> Config.metadataName
    }
