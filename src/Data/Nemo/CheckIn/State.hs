{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.CheckIn.State where

import           Control.Lens       (makeLenses)
import           Crypto.Hash.SHA256 (Ctx)

data State = State
  { _hashCtx :: Ctx
  , _exports :: [String]
  }

makeLenses ''State
