module Nemo.Parser where

import           Data.Char          (isSpace)
import           Text.Parsec        (many1, satisfy)
import           Text.Parsec.Char   (alphaNum, spaces, string)
import           Text.Parsec.String (Parser)

nemo :: Parser ()
nemo = string "#" *> spaces *> string "nemo" *> return ()

-- TODO(larioj): make this better
filePath :: Parser FilePath
filePath = many1 $ satisfy (not . isSpace)

-- TODO(larioj): make this better
identifier :: Parser String
identifier = many1 alphaNum
