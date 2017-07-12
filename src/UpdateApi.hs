module UpdateApi where

import           File
import qualified HaskellUpdate
import           Nemo

makeClone :: Nemo String File -> String -> (String, File)
makeClone = HaskellUpdate.makeClone
