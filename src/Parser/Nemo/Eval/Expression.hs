module Parser.Nemo.Eval.Expression where

import           Data.Nemo.Eval.Expression (Expression (Copy, Move))
import           Nemo.Util                 (silence)
import           Parser.Nemo               (filePath, identifier, nemo)
import           Text.Parsec               (many1, parse, (<|>))
import           Text.Parsec.Char          (space, spaces, string)
import           Text.Parsec.String        (Parser)

fromString :: String -> Maybe Expression
fromString = silence . parse expression "LINE"

expression :: Parser Expression
expression =
  combine <$> spaces <*> nemo <*> many1 space <*> string "include" <*>
  many1 space <*>
  paren (copy <|> move) <*>
  many1 space <*>
  identifier <*>
  spaces
  where
    combine _ _ _ _ _ fn _ ident _ = fn ident

paren :: Parser a -> Parser a
paren p = string "(" *> spaces *> p <* spaces <* string ")"

copy :: Parser (String -> Expression)
copy = Copy <$> (string "copy" *> many1 space *> filePath)

move :: Parser (String -> Expression)
move = Move <$> (string "move" *> many1 space *> filePath)
