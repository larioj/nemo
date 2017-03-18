module NemoLib.ExtractPointer where

import           NemoLib.ShadowNode

extractPointer :: ShadowNode -> (String, String)
extractPointer (ShadowNode k _ v) = (k, v)
