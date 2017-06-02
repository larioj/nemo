module GraphSpec (spec) where

import Test.Hspec
import Graph
    ( topoSort
    , invert
    , graph
    )
import Examples

spec :: Spec
spec = do
    describe "The topoSort method" $ do
        it "should topologically sort a Graph" $ do
            topoSort graphA `shouldBe` (e : a : c : b : d : [])

    describe "The invert method" $ do
        it "should invert a graph" $ do
            invert graphA `shouldBe` graphAInverse
