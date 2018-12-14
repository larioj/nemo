{-# LANGUAGE OverloadedStrings #-}

module Parser.Nemo.Directive where

import           Control.Lens        (Prism', prism')
import           Data.Nemo.Directive (Directive (Content, Export, Include))
import           Nemo.Clang          (tokenize)
import           Nemo.Util           (silence)
import           Parser.Nemo         (identifier, nemo)
import           Parser.Nemo.Name    (name)
import qualified Parser.Nemo.Name    as Name
import           Text.Parsec         (ParseError, many, many1, parse, (<|>))
import           Text.Parsec.Char    (anyChar, space, spaces, string)
import           Text.Parsec.String  (Parser)

_Directive :: Prism' String Directive
_Directive = prism' toString fromString

fromString :: String -> Maybe Directive
fromString = silence . parseDirective

toString :: Directive -> String
toString directive =
  case directive of
    Include name alias -> unwords ["#nemo include", Name.toString name, alias]
    Export alias prefix -> unwords ["#nemo export", alias, prefix]
    Content tokens -> concat tokens

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
