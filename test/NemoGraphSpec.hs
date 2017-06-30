module NemoGraphSpec where

import Test.Hspec
import Graph
import NemoGraph
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Arrow

depGraphA =
    ("a", "b" : "c" : []) :
    ("b", "c" : []) :
    ("d", "b" : []) :
    ("e", "c" : []) :
    []

nemoGraphA =
    NemoGraph (graph depGraphA) Map.empty Map.empty

nemoGraphB =
    NemoGraph (graph depGraphA) Map.empty (Map.fromList [("c", Just "c-clone")])

spec :: Spec
spec = do
    describe "The update method" $ do
        it "should update a nemo graph" $ do
            update "c" "c-clone" nemoGraphA `shouldBe`
                (insertClone "c" (Just "c-clone") >>>
                 insertDependencies "c-clone" Set.empty >>>
                 insertPredecessor "c-clone" Nothing) nemoGraphA

        it "should update a nemo graph when a clone is already present" $ do
            update "b" "b-clone" nemoGraphB `shouldBe`
                (insertClone "b" (Just "b-clone") >>>
                 insertDependencies "b-clone" (Set.fromList ["c-clone"]) >>>
                 insertPredecessor "b-clone" Nothing) nemoGraphB
