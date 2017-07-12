module Update where

import           Config
import           File
import           Nemo
import           UpdateApi

update :: Nemo FilePath File -> Nemo FilePath File
update = Nemo.update (moveToConfigDir makeClone)

moveToConfigDir :: (Nemo String File -> String -> (String, File))
                 -> Nemo String File -> String -> (String, File)
moveToConfigDir fn nemo s =
    (id, replaceSubdirectoryPart configDir file)
    where
        (id, file) = fn nemo s
