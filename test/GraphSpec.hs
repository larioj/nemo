module GraphSpec where

import qualified Data.Set   as Set
import           Graph
import           Test.Hspec
import           Util

topoSortProperty :: Ord k => Graph k -> [k] -> Bool
topoSortProperty g sorted =
    (flip all) sorted $ \element ->
    (flip all) (allBefore element sorted) $ \before ->
        not $ isSuccessor g before element

graphA = graph
    ( ("a", "b" : "c" : []) :
      ("b", "c" : []) :
      ("d", "b" : []) :
      ("e", "c" : []) :
      []
    )

spec :: Spec
spec = do
    describe "The successors method" $ do
        it "should return all the succesors of an element in a graph" $ do
            successors graphA "d" `shouldBe` Set.fromList ("d" : "b" : "c" : [])

    describe "The isSuccessor method" $ do
        it "should return true when an element is a successor" $ do
            isSuccessor graphA "c" "d" `shouldBe` True
        it "should return false when an element is not a successor" $ do
            isSuccessor graphA "d" "c" `shouldBe` False

    describe "The topoSort method" $ do
        it "should return a list with the topological sort property" $ do
            topoSortProperty graphA (topoSort graphA)
