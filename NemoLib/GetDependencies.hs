module NemoLib.GetDependencies where

import           NemoLib.NemoNode

getDependencies :: NemoNode -> [FilePath]
getDependencies (NemoNode _ _ d) = d
