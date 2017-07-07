module NemoSpec where

import Test.Hspec
import Nemo
import File
import NemoGraph
import qualified Data.Map as Map
import qualified Data.Set as Set
import HaskellTransform

file1 =
    File
        { name = "Spec"
        , extension = ".hs"
        , directory = ""
        , moduleRoot = "test/"
        , projectRoot = "/Users/jlariosmurillo/Hobby/Graph"
        , contents = "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}\n"
        }
rep1 =
    Map.fromList [("Spec.hs", file1)]
deps1 =
    Map.fromList [("Spec.hs", Set.empty)]
nemoGraph1 =
    NemoGraph
        { dependencyGraph = deps1
        , predecessorGraph =  Map.empty
        , cloneGraph =  Map.empty
        }
nemo1 =
    Nemo
        { representationMap = rep1
        , nemoGraph = nemoGraph1
        }

spec :: Spec
spec = do
    describe "A misbehaving file" $ do
        it "should not fail" $ do
            extractModuleDeclaration "Spec" (contents file1) `shouldBe` Nothing
