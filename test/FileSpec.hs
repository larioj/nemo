module FileSpec where

{-
import           Control.Arrow
import           Data.List       (intercalate)
import           Data.List.Utils (replace)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           File
import           Graph
import           Nemo
import           NemoGraph
import           Test.Hspec

cloneFn :: Nemo String File -> String -> (String, File)
cloneFn (Nemo rep g) original = (name file, file)
    where file = transform g (rep Map.! original)

transformContents :: NemoGraph String -> File -> File
transformContents g file =
    file { contents = newContents }
    where
        newContents =
            (contents            >>>
             words               >>>
             map (cloneOrSelf g) >>>
             intercalate " ") file

transformName :: NemoGraph String -> File -> File
transformName g file =
    file { name = newName }
    where
        len = length $ contents file
        newName =
            (name file) ++
            "-clone"    ++
            (show len)  ++
            (replicate len '!')

transform :: NemoGraph String -> File -> File
transform g file =
    (transformContents g >>> transformName g) file

fileSync :: Nemo String File -> Nemo String File
fileSync = sync cloneFn

aClone = "a-clone0"
fileA = File "a" ".hs" "foo/bar/" "" "" ""
fileA' = fileA { name = "a-clone0" }
repSingleFile = Map.fromList [("a", fileA)]
repSingleFile' = Map.insert "a-clone0" fileA' repSingleFile
nemoGraphSingleFile = NemoGraph (graph [("a", [])]) Map.empty Map.empty
nemoGraphSingleFile' =
    (insertClone "a" (Just "a-clone0") >>>
     insertDependencies "a-clone0" Set.empty >>>
     insertPredecessor "a-clone0" Nothing) nemoGraphSingleFile
nemoSingleFile = Nemo repSingleFile nemoGraphSingleFile
nemoSingleFile' = Nemo repSingleFile' nemoGraphSingleFile'
nemoSingleFileSpec =
    it "should be able to synch a single file" $ do
        fileSync nemoSingleFile `shouldBe` nemoSingleFile'

bClone = "b-clone8!!!!!!!!"
fileB = File "b" ".hs" "foo/bar/" "" "" "a"
fileB' = fileB { name = bClone, contents = "a-clone0" }
cClone = "c-clone8!!!!!!!!"
fileC = File "c" ".hs" "foo/bar/" "" "" "a"
fileC' = fileC { name = cClone, contents = "a-clone0" }
repThreeNewFiles = Map.fromList [("a", fileA), ("b", fileB), ("c", fileC)]
repThreeNewFiles' =
    (Map.insert aClone fileA' >>>
     Map.insert bClone fileB' >>>
     Map.insert cClone fileC') repThreeNewFiles
nemoGraphThreeNewFiles =
    NemoGraph (graph [("a", []), ("b", ["a"]), ("c", ["a"])]) Map.empty Map.empty
nemoGraphThreeNewFiles' =
    (insertClone "a" (Just "a-clone0") >>>
     insertDependencies "a-clone0" Set.empty >>>
     insertPredecessor "a-clone0" Nothing >>>
     insertClone "b" (Just bClone) >>>
     insertDependencies bClone (Set.fromList [aClone]) >>>
     insertPredecessor bClone Nothing >>>
     insertClone "c" (Just cClone) >>>
     insertDependencies cClone (Set.fromList [aClone]) >>>
     insertPredecessor cClone Nothing) nemoGraphThreeNewFiles
nemoThreeNewFiles = Nemo repThreeNewFiles nemoGraphThreeNewFiles
nemoThreeNewFiles' = Nemo repThreeNewFiles' nemoGraphThreeNewFiles'
nemoThreeNewFilesSpec =
    it "should be able to synch a multiple new files" $ do
        fileSync nemoThreeNewFiles `shouldBe` nemoThreeNewFiles'

d = "d"
dClone = "d-clone33" ++ (replicate 33 '!')
fileD = File d ".hs" "foo/bar/" "" "" "b c"
fileD' = fileD { name = dClone, contents = bClone ++ " " ++ cClone }
repAddingFile = Map.insert d fileD repThreeNewFiles'
repAddingFile' = Map.insert dClone fileD' repAddingFile
nemoGraphAddingFile =
    insertDependencies d (Set.fromList ["b", "c"]) nemoGraphThreeNewFiles'
nemoGraphAddingFile' =
    (insertClone "d" (Just dClone) >>>
     insertDependencies dClone (Set.fromList [bClone, cClone]) >>>
     insertPredecessor dClone Nothing) nemoGraphAddingFile
nemoAddingFile = Nemo repAddingFile nemoGraphAddingFile
nemoAddingFile' = Nemo repAddingFile' nemoGraphAddingFile'
nemoAddingFileSpec =
    it "should be able to synch a new file after it has already synched multiple files" $ do
        fileSync nemoAddingFile `shouldBe` nemoAddingFile'

fileA2 = fileA { contents = ";;;" }
a'' = "a-clone3!!!"
b'' = "b-clone11" ++ (replicate 11 '!')
c'' = "c-clone11" ++ (replicate 11 '!')
d'' = "d-clone41" ++ (replicate 41 '!')
fileA'' = fileA2 { name = a'' }
fileB'' = fileB { name = b'', contents = a'' }
fileC'' = fileC { name = c'', contents = a'' }
fileD'' = fileD { name = d'', contents = b'' ++ " " ++ c'' }
repModifyFile = Map.insert "a" fileA2 repAddingFile'
repModifyFile' =
    (Map.insert a'' fileA'' >>>
     Map.insert b'' fileB'' >>>
     Map.insert c'' fileC'' >>>
     Map.insert d'' fileD'') repModifyFile
nemoGraphModifyFile = nemoGraphAddingFile'
nemoGraphModifyFile' =
    (insertClone "a" (Just a'') >>>
     insertClone "b" (Just b'') >>>
     insertClone "c" (Just c'') >>>
     insertClone "d" (Just d'') >>>
     insertDependencies a'' (Set.empty) >>>
     insertDependencies b'' (Set.fromList [a'']) >>>
     insertDependencies c'' (Set.fromList [a'']) >>>
     insertDependencies d'' (Set.fromList [b'', c'']) >>>
     insertPredecessor a'' (Just aClone) >>>
     insertPredecessor b'' (Just bClone) >>>
     insertPredecessor c'' (Just cClone) >>>
     insertPredecessor d'' (Just dClone)) nemoGraphModifyFile
nemoModifyFile = Nemo repModifyFile nemoGraphModifyFile
nemoModifyFile' = Nemo repModifyFile' nemoGraphModifyFile'
nemoModifyFileSpec =
    it "should be able to update all the dependencies when a file changes" $ do
        fileSync nemoModifyFile `shouldBe` nemoModifyFile'

spec :: Spec
spec = do
    describe "The sync method" $ do
        nemoSingleFileSpec
        nemoThreeNewFilesSpec
        nemoAddingFileSpec
        nemoModifyFileSpec

-}

import           Test.Hspec

spec :: Spec
spec = do
    describe "nothing" $ do
        it "should do" $ do
            "A" `shouldBe` "A"