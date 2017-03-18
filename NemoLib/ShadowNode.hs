module NemoLib.ShadowNode where

data ShadowNode = ShadowNode String -- shadow address
                             String -- nemo address
                             String -- shadow contents
                  deriving (Show, Eq, Ord)
