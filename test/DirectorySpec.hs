module DirectorySpec where

import Test.Hspec
import Directory

spec :: Spec
spec = do
    describe "The listDirectoryRecursively method" $ do
        it "should list all files under a directory" $ do
            pending
