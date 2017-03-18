module NemoLib.ExtractPointer where

import           NemoLib.ShadowNode

extractPointer :: ShadowNode -> (String, String)
extractPointer (ShadowNode hash name _) = (hash, name)
