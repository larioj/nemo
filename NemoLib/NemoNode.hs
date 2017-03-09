module NemoLib.NemoNode where

data NemoNode = NemoNode FilePath String [FilePath]
                deriving (Show, Eq, Ord)
