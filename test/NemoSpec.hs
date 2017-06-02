module NemoSpec where

import Test.Hspec
import Examples
import Nemo
    ( sync
    , successors
    )

shadow _ k = (k', k')
    where k' = k ++ "-clone"
syncShadow = sync shadow

spec :: Spec
spec = do
    describe "The sync method" $ do
        it "should leave an already synched state unchanged" $ do
            syncShadow nemoA `shouldBe` (syncShadow . syncShadow $ nemoA)

    describe "The successors method" $ do
        it "should return the list of all decendents of an element" $ do
            successors graphB a `shouldBe` aSuccessorsGraphB
