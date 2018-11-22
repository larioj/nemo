module Nemo.Clang where

import           Data.Char (isAlphaNum, isSpace)
import           Data.List (groupBy)

tokenize :: String -> [String]
tokenize = groupBy sameTokenClass

sameTokenClass :: Char -> Char -> Bool
sameTokenClass a b =
  (isSpace a && isSpace b) ||
  (isIdentifier a && isIdentifier b) || (isOperator a && isOperator b)

isIdentifier :: Char -> Bool
isIdentifier '_' = True
isIdentifier c   = isAlphaNum c

isOperator :: Char -> Bool
isOperator c = not (isSpace c || isIdentifier c)
