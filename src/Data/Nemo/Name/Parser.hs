module Data.Nemo.Name.Parser where

import           Control.Lens       (Prism', prism')
import           Data.Nemo.Name     (Name (Name), _hash, _prefix)
import           Nemo.Util          (silence)
import           Text.Parsec        (many, many1, oneOf, parse, try, (<|>))
import           Text.Parsec.Char   (alphaNum)
import           Text.Parsec.String (Parser)

sep :: String
sep = "_"

name :: Parser Name
name = do
  leading <- many $ oneOf "_"
  first <- alphaNumThenUnderscore
  rest <- many1 (try alphaNumThenUnderscore <|> many1 alphaNum)
  return $ Name (init (leading ++ first ++ (concat $ init rest))) (last rest)
  where
    alphaNumThenUnderscore = (++) <$> many1 alphaNum <*> (many1 $ oneOf "_")

_Name :: Prism' String Name
_Name = prism' toString fromString

fromString :: String -> Maybe Name
fromString = silence . parse name "LINE"

toString :: Name -> String
toString n = _prefix n <> sep <> _hash n
