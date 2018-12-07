module Data.Nemo.Name.Parser where

import           Data.Nemo.Name     (Name (Name))
import           Nemo.Parser        (identifier)
import           Text.Parsec.Char   (string)
import           Text.Parsec.String (Parser)

name :: Parser Name
name = Name <$> identifier <*> (string "_" *> identifier)
