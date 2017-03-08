module NemoLib.ShadowNode where

import           NemoLib.File
import           NemoLib.NemoNode

data ShadowNode =
    ShadowNode { nemoNode  :: NemoNode
               , nemoPointer :: [FilePath]
               } deriving (Show, Eq)
