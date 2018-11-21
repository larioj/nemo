module Data.Nemo.Name where

data Name
  = Name
      { prefix :: String
      , hash :: String
      }
  deriving (Show, Eq, Ord)
