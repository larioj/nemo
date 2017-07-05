module Update where

import File
import Nemo
import HaskellTransform as HT

-- TODO: multiplexes the updates for different languages
update :: Nemo FilePath File -> Nemo FilePath File
update =
    Nemo.sync HT.makeClone
