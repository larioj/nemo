module NemoLib.ExtractDependencies where

import           Text.Regex.Posix

extractDependencies :: String -> [String]
extractDependencies s =
    map (\m -> m !! 1) (s =~ "import[ \t]+NemoLib\\.(.+)")
