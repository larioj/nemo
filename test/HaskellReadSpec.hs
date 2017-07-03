module HaskellReadSpec where

import Test.Hspec
import HaskellRead
import File

file = File "" "" "" ""

fileA =
    file {
        contents =
            "module FileA where\n\n" ++
            "import Foo.Bar.NemoA as A\n" ++
            "import qualified NemoB as B\n\n" ++
            "fileA = undefined\n"
     }

spec :: Spec
spec = do
    describe "The extractDependencies method" $ do
        it "should extract the dependencies of a file" $ do
            extractDependencies fileA
                `shouldBe` ["Foo/Bar/NemoA.hs", "NemoB.hs"]

    describe "The extractImportExpressions method" $ do
        it "should extract import exp in file" $ do
            extractImportExpressions fileA
                `shouldBe` ["import Foo.Bar.NemoA","import qualified NemoB"]
