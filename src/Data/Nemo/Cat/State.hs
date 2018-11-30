{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Cat.State where

import           Control.Lens   (makeLenses)
import           Data.Map       (Map)
import           Data.Nemo.Name (Name)
import           Data.Set       (Set)

data State = State
  { _names :: [Map String Name]
  , _seen  :: Set Name
  }

makeLenses ''State
