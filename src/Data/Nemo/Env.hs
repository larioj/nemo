{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Env where

import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Nemo.Error        (maybeDie)
import qualified Data.Nemo.Error        as Err
import           Nemo.Util              (findMarker)
import           System.FilePath        (takeDirectory, (</>))

data Env = Env
  { _base     :: FilePath
  , _nemo     :: FilePath
  , _sources  :: FilePath
  , _metadata :: FilePath
  } deriving (Show, Eq, Ord)

makeLenses ''Env

env :: MonadIO m => String -> m Env
env marker = do
  nemo <- maybeDie Err.UnableToFindMarker =<< liftIO (findMarker marker)
  return $
    Env
      { _base = takeDirectory nemo
      , _nemo = nemo
      , _sources = nemo </> "src"
      , _metadata = nemo </> "metadata"
      }
