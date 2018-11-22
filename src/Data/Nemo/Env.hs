{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Env where

import           Control.Lens    (makeLenses)
import           Data.Maybe      (fromMaybe)
import           Nemo.Util       (findMarker)
import           System.FilePath (takeDirectory, (</>))

data Env = Env
  { _base    :: FilePath
  , _nemo    :: FilePath
  , _sources :: FilePath
  } deriving (Show, Eq, Ord)

makeLenses ''Env

env :: String -> IO Env
env marker = do
  nemo <- fromMaybe <$> fail "Unable to find marker" <*> findMarker marker
  return $
    Env {_base = takeDirectory nemo, _nemo = nemo, _sources = nemo </> "src"}
