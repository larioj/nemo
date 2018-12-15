{-# LANGUAGE OverloadedStrings #-}

module Parser.Nemo.Directive where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Nemo.Directive    (Directive (Content, Export, Include))
import qualified Data.Nemo.Error        as Err
import           Nemo.Clang             (tokenize)
import           Parser.Nemo            (identifier, nemo)
import           Parser.Nemo.Name       (name)
import qualified Parser.Nemo.Name       as Name
import           Text.Parsec            (many, many1, try, (<|>))
import           Text.Parsec.Char       (anyChar, space, spaces, string)
import           Text.Parsec.String     (Parser)

parseOrDie :: MonadIO m => String -> Int -> String -> m Directive
parseOrDie file lineNum source = Err.parseOrDie directive file lineNum source

toString :: Directive -> String
toString directive =
  case directive of
    Include name alias -> unwords ["#nemo include", Name.toString name, alias]
    Export alias prefix -> unwords ["#nemo export", alias, prefix]
    Content tokens -> concat tokens

directive :: Parser Directive
directive = do
  e <- try (fmap Right $ spaces *> nemo) <|> (fmap Left $ content)
  case e of
    Left c  -> return c
    Right _ -> many1 space *> (include <|> export) <* spaces

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
