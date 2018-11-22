{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Checkin where

import           Control.Lens (makeLenses)

data Checkin = Checkin
  { _path :: FilePath
  } deriving (Show, Eq, Ord)

makeLenses ''Checkin
