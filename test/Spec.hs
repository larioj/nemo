module Spec where

import qualified GraphTest
import qualified NemoTest

main =
    GraphTest.runTest >>
    putStrLn "" >>
    NemoTest.runTest
