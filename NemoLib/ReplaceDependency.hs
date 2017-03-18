module NemoLib.ReplaceDependency where

import Text.Regex(subRegex, mkRegex)
import NemoLib.EscapeRegex

-- replace NemoLib.<key> with NemoLib.ShadowLib.<value>

replaceDependency :: String -> (String, String) -> String
replaceDependency contents (key, value) =
    subRegex (mkRegex (escapeRegex keyText)) contents valueText
    where keyText = "NemoLib." ++ key
          valueText = "NemoLib.ShadowLib." ++ value
    
