module HaskellTransformSpec where

import qualified Data.Map         as Map
import           File
import           Hash
import           HaskellTransform
import           NemoPath
import           Read
import           Test.Hspec

pathA = makeNemoPath "/usr/someusr" "sub" "FileA.hs"
fileA =
    makeFile
        pathA $
        "module FileA where\n\n" ++
        "import Foo.Bar.FileB as A\n" ++
        "import qualified NemoB as B\n\n" ++
        "fileA = undefined\n"
{-
fileA2 =
    makeFile
        pathA $
        "module FileA where\n\n" ++
        "import Foo.Bar.FileB as A\n" ++
        "import qualified NemoB as B\n\n" ++
        "fileA = undefined\n" ++
        "fileA2 = FileA.fileA\n"
-}

pathB = makeNemoPath "/usr/someusr" "" "Foo/Bar/FileB.hs"
fileB =
    makeFile
        pathB $
        "module Foo.Bar.FileB\n\n" ++
        "nemoA = undefined\n"

hashB = base16AlphaHash $ contents fileB
pathB' = makeNemoPath "/usr/someusr" "" ("Foo/Bar/FileB_" ++ hashB ++ ".hs")
fileB' =
    makeFile
        pathB' $
            "module Foo.Bar.FileB_" ++ hashB ++ "\n\n" ++
            "nemoA = undefined\n"

contA' =
    "module FileA where\n\n" ++
    "import Foo.Bar.FileB_" ++ hashB ++ " as A\n" ++
    "import qualified NemoB as B\n\n" ++
    "fileA = undefined\n"
hashA = base16AlphaHash contA'
pathA' = makeNemoPath "/usr/someusr" "sub" ("FileA_" ++ hashA ++ ".hs")
fileA' =
    makeFile
        pathA' $
        "module FileA_" ++ hashA ++ " where\n\n" ++
        "import Foo.Bar.FileB_" ++ hashB ++ " as A\n" ++
        "import qualified NemoB as B\n\n" ++
        "fileA = undefined\n"

nemo = getNemo' [fileA, fileB] Map.empty Map.empty
nemo2 = getNemo' [fileA, fileB, fileB'] Map.empty $
    Map.fromList [(identifier fileB, Just $ identifier fileB')]

badFile =
    makeFile
        (makeNemoPath "/Users/jlariosmurillo/Hobby/Graph" "test/" "Spec.hs") $
        "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n"

spec :: Spec
spec = do
    describe "The replaceModuleWithHash method" $ do
        it "should append hash to module name" $ do
            shouldBe (replaceModuleWithHash fileA) $
                let hash = base16AlphaHash $ contents fileA in
                makeFile
                    (makeNemoPath "/usr/someusr" "sub" ("FileA_" ++ hash ++ ".hs")) $
                    "module FileA_" ++ hash ++ " where\n\n" ++
                    "import Foo.Bar.FileB as A\n" ++
                    "import qualified NemoB as B\n\n" ++
                    "fileA = undefined\n"

        it "should not fail when given a bad file" $ do
            shouldBe (replaceModuleWithHash badFile) $
                let hash = base16AlphaHash $ contents badFile in
                makeFile
                    (makeNemoPath
                        "/Users/jlariosmurillo/Hobby/Graph"
                        "test/"
                        $ "Spec_" ++ hash ++ ".hs") $
                    "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n"

{- TODO: This should pass
        it "should replace self references in methods" $ do
            shouldBe (replaceModuleWithHash fileA2) $
                let hash = base16AlphaHash $ contents fileA2 in
                makeFile
                    (makeNemoPath "/usr/someusr" "sub" ("FileA_" ++ hash ++ ".hs")) $
                    "module FileA_" ++ hash ++ " where\n\n" ++
                    "import Foo.Bar.FileB as A\n" ++
                    "import qualified NemoB as B\n\n" ++
                    "fileA = undefined\n" ++
                    "fileA2 = FileA_" ++ hash ++ ".fileA\n"
-}
    describe "The makeClone method" $ do
        it "should be able to create a correct clone" $ do
            shouldBe (snd $ makeClone nemo $ identifier fileB) $
                fileB'

        it "should be able to create a correct clone with dependencies" $ do
            shouldBe (snd $ makeClone nemo2 $ identifier fileA) $
                fileA'
