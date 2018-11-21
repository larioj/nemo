module Data.Nemo.Checkin where

data Checkin
  = Checkin
      { path :: FilePath
      }
  deriving (Show, Eq, Ord)
