{-# LANGUAGE OverloadedStrings #-}

module Data.Nemo.Directive.Parser where

import           Control.Lens          (Prism', prism', re, (^.))
import           Data.Nemo.Directive   (Directive (Content, Export, Include))
import           Data.Nemo.Name        (_Name)
import           Data.Nemo.Name.Parser (name)
import           Nemo.Clang            (tokenize)
import           Nemo.Parser           (identifier, nemo)
import           Nemo.Util             (silence)
import           Text.Parsec           (ParseError, many, many1, parse, (<|>))
import           Text.Parsec.Char      (anyChar, space, spaces, string)
import           Text.Parsec.String    (Parser)

_Directive :: Prism' String Directive
_Directive = prism' toString fromString

fromString :: String -> Maybe Directive
fromString = silence . parseDirective

toString :: Directive -> String
toString directive =
  case directive of
    Include name alias  -> unwords ["#nemo include", name ^. re _Name, alias]
    Export alias prefix -> unwords ["#nemo export", alias, prefix]
    Content tokens      -> concat tokens

parseDirective :: String -> Either ParseError Directive
parseDirective = parse directive "LINE"

directive :: Parser Directive
directive =
  spaces *> (nemo *> many1 space *> (include <|> export) <|> content) <* spaces

include :: Parser Directive
include =
  Include <$> (string "include" *> many1 space *> name) <*>
  (many1 space *> identifier)

export :: Parser Directive
export =
  Export <$> (string "export" *> many1 space *> identifier) <*>
  (many1 space *> identifier)

-- TODO(larioj): better tokenizer
content :: Parser Directive
content = Content . tokenize <$> many anyChar
