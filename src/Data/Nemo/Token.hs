{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Token where

import           Control.Lens (makeLenses)

data Token
  = Space { _content :: String }
  | Newline { _content :: String }
  | Identifier { _content :: String }
  | Operator { _content :: String }

makeLenses ''Token
