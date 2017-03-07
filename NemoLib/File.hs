module NemoLib.File where

data File = File { name     :: String
                 , contents :: String
                 } deriving (Show, Eq)
