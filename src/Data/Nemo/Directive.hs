{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Directive where

import           Control.Lens   (makeLenses)
import           Data.Nemo.Name (Name)

data Directive
  = Include { _name  :: Name
            , _alias :: String }
  | Export { _alias  :: String
           , _prefix :: String }
  | Content { _tokens :: [String] }
  deriving (Show, Eq)

makeLenses ''Directive
