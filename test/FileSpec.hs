module FileSpec where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List.Utils
    ( replace
    )
import Test.Hspec
import Graph
    ( Graph
    , graph
    )
import File
    ( File (..)
    )
import Nemo
    ( Nemo
    , sync
    , getDependencyClone
    , empty
    )

transform :: Graph String -> Graph String -> File -> File
transform deps clones (File name ext dir contents) =
    (File newName ext dir newContents)
    where
        dependencies = Set.toList . Map.findWithDefault Set.empty name $ deps
        replacements = map (\d -> (d, getDependencyClone clones d)) dependencies
        replaceDep contents (dep, clone) = replace dep clone contents
        newContents = foldl replaceDep contents replacements
        len = length newContents
        newName = name ++ "-clone" ++ (replicate len '!') ++ (show len)

adapter :: Nemo String File -> String -> (String, File)
adapter (reps, deps, _, clones) k = (name file, file)
    where file = transform deps clones (reps Map.! k)

fileSync :: Nemo String File -> Nemo String File
fileSync = sync adapter

-- scenario: a single new file
a = "a"
fileA = File a ".hs" "foo/bar/" ""
rep1 = Map.fromList [(a, fileA)]
deps1 = graph [(a, [])]
nemo1 :: Nemo String File
nemo1 = (rep1, deps1, Map.empty, Map.empty)
-- end scenario

-- scenario: three new files with dependencies
b = "b"
fileB = File b ".hs" "foo/bar/" a
c = "c"
fileC = File c ".hs" "foo/bar/" a
rep2 = Map.fromList [(a, fileA), (b, fileB), (c, fileC)]
deps2 = graph [(b, [a]), (c, [a])]
nemo2 :: Nemo String File
nemo2 = (rep2, deps2, Map.empty, Map.empty)
-- end scenario

-- scenario: adding a new file on an already synched state
d = "d"
fileD = File d ".hs" "foo/bar/" "b c"
(rep3, deps3, preds3, clones3) = fileSync nemo2
rep4 = Map.insert d fileD rep3
deps4 = Map.insert d (Set.fromList [b, c]) deps3
nemo4 :: Nemo String File
nemo4 = (rep4, deps4, preds3, clones3)
-- end scenario

-- scenario: modifying a file
fileA' = File a ".hs" "foo/bar/" ";;;"
(rep5, deps5, preds5, clones5) = fileSync nemo4
rep6 = Map.insert a fileA' rep5
nemo6 :: Nemo String File
nemo6 = (rep6, deps5, preds5, clones5)
-- end scenario

spec :: Spec
spec = do
    describe "The sync method on files" $ do
        it "should do stuff" $ do
            fileSync nemo6 `shouldBe` empty
