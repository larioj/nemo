module NemoLib.NemoNode where

import           NemoLib.File

data NemoNode =
    NemoNode { address      :: String
             , contents     :: String
             , dependencies :: [String]
             } deriving (Show, Eq)
