module NemoLib.GetName where

import NemoLib.ShadowNode

getName :: ShadowNode -> String  
getName (ShadowNode _ name _) = name  
