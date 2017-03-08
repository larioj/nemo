module NemoLib.File where

data File =
    File { path     :: FilePath
         , contents :: String
         } deriving (Show, Eq)
