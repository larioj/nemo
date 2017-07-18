module Write where

import           Config
import qualified Data.Map  as Map
import           File
import           Nemo
import           NemoGraph

writeNemo :: FilePath -> Nemo FilePath File -> IO ()
writeNemo project (Nemo rep g) =
    dumpAll (Map.elems rep) >>
    writeCloneGraph project (cloneGraph g) >>
    writePredecessorGraph project (predecessorGraph g)


