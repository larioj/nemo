module UpdateApi where

import           Config
import           File
import qualified           HaskellUpdate
import           Nemo

-- TODO: multiplexes the updates for different languages
update :: Nemo FilePath File -> Nemo FilePath File
update =
    Nemo.sync (moveToNemoLib HaskellUpdate.makeClone)

moveToNemoLib :: (Nemo String File -> String -> (String, File))
               -> Nemo String File -> String -> (String, File)
moveToNemoLib fn nemo s =
    (id, replaceSubdirectoryPart configDir file)
    where
        (id, file) = fn nemo s
