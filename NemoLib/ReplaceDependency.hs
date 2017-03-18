module NemoLib.ReplaceDependency where

import           NemoLib.EscapeRegex
import           Text.Regex          (mkRegex, subRegex)

-- replace NemoLib.<name> with NemoLib.ShadowLib.<hash_name>

replaceDependency :: String -> (String, String) -> String
replaceDependency contents (hash, name) =
    subRegex (mkRegex (escapeRegex nameText)) contents hashText
    where nameText = "NemoLib." ++ name
          hashText = "NemoLib.ShadowLib." ++ hash ++ "_" ++ name

