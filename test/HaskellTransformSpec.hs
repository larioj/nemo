module HaskellTransformSpec where

import Test.Hspec
import HaskellTransform
import File
import qualified Data.Map as Map
import Graph
import NemoGraph
import Nemo

contents1 =
    "import Nemo \n" ++
    "import qualified Module.Foo as Foo \n" ++
    "import  \t  Module.Bar hiding (bar)\n" ++
    "import Baar\n" ++
    "import qualified Baar as B\n"

contents2 =
    "import Nemo \n" ++
    "import Bar"
contents2' =
    "import NNeemmoo \n" ++
    "import Bar"

file = File "" "" "" ""

nemoA =
    file {
        name = "NemoA",
        extension = ".hs",
        directory = "",
        contents =
            "module NemoA where\n\n" ++
            "nemoA = undefined\n"
    }

nemoB =
    file {
        name = "NemoB",
        extension = ".hs",
        directory = "",
        contents =
            "module NemoB where \n\n" ++
            "import qualified NemoA as A\n\n" ++
            "nemoB = undefined\n"
    }

rep = Map.fromList [(identifier nemoA, nemoA), (identifier nemoB, nemoB)]
g = NemoGraph
    (graph [(identifier nemoA, []), (identifier nemoB, [identifier nemoA])])
    Map.empty
    Map.empty
nemo = Nemo rep g

spec :: Spec
spec = do
    describe "The importRegex method" $ do
        it "should extract a haskell import" $ do
            extractImports "Nemo" contents1
                `shouldBe` ["import Nemo"]

        it "should extract a haskell import with qualified" $ do
            extractImports "Module.Foo" contents1
                `shouldBe` ["import qualified Module.Foo"]

        it "should extract a haskell import with weird spacing" $ do
            extractImports "Module.Bar" contents1
                `shouldBe` ["import  \t  Module.Bar"]

        it "should extract a haskell import multiple times" $ do
            extractImports "Baar" contents1
                `shouldBe` ["import Baar", "import qualified Baar"]

    describe "The replaceImport method" $ do
        it "should replace the name of an import" $ do
            replaceImport "Nemo" "NNeemmoo" contents2
                `shouldBe` contents2'

    describe "The makeClone method" $ do
        it "should clone a file" $ do
            makeClone nemo (identifier nemoA) `shouldBe` ("", file)
