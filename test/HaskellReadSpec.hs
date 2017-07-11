module HaskellReadSpec where

import           File
import           HaskellRead
import           NemoPath
import           Test.Hspec

pathA = (makeNemoPath "/usr/someusr" "sub" "FileA.hs")
fileA =
    makeFile
        pathA $
        "module FileA where\n\n" ++
        "import Foo.Bar.NemoA as A\n" ++
        "import qualified NemoB as B\n\n" ++
        "fileA = undefined\n"

spec :: Spec
spec = do
    describe "The isSupportedFilePath method" $ do
        it "should return True for haskell file paths" $ do
            isSupportedFilePath (toFilePath pathA) `shouldBe` True

    describe "The isSupportedFile method" $ do
        it "should return True for haskell files" $ do
            isSupportedFile fileA `shouldBe` True

    describe "The extractDependencies method" $ do
        it "should extract the dependencies of a file" $ do
            extractDependencies fileA
                `shouldBe` ["Foo/Bar/NemoA.hs", "NemoB.hs"]
