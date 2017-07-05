module Update where

import File
import Nemo
import NemoConfig
import HaskellTransform as Ht

-- TODO: multiplexes the updates for different languages
update :: Nemo FilePath File -> Nemo FilePath File
update =
    Nemo.sync (moveToNemoLib Ht.makeClone)

moveToNemoLib :: (Nemo String File -> String -> (String, File))
               -> Nemo String File -> String -> (String, File)
moveToNemoLib fn nemo s =
    (id, file { moduleRoot = configDir })
    where
        (id, file) = fn nemo s
