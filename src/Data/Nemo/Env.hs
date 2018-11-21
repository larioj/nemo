module Data.Nemo.Env where

data Env
  = Env
      { base :: FilePath
      , sources :: FilePath
      }
  deriving (Show, Eq, Ord)
