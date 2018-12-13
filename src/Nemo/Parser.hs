module Nemo.Parser where

import           Control.Applicative (liftA2)
import           Data.Char           (isAlphaNum, isSpace)
import           Text.Parsec         (many1, satisfy)
import           Text.Parsec.Char    (spaces, string)
import           Text.Parsec.String  (Parser)

nemo :: Parser ()
nemo = string "#" *> spaces *> string "nemo" *> return ()

-- TODO(larioj): make this better
filePath :: Parser FilePath
filePath = many1 $ satisfy $ liftA2 (&&) (not . isSpace) (flip notElem "()[]<>")

-- TODO(larioj): make this better
identifier :: Parser String
identifier = many1 $ satisfy $ (||) <$> isAlphaNum <*> flip elem "_"
