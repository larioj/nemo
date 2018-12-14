module Data.Nemo.Name.Parser where

import           Control.Monad.IO.Class (MonadIO)
import qualified Data.Nemo.Error        as Err
import           Data.Nemo.Name         (Name (Name), _hash, _prefix)
import           Text.Parsec            (many, many1, oneOf, try, (<|>))
import           Text.Parsec.Char       (alphaNum)
import           Text.Parsec.String     (Parser)

sep :: String
sep = "_"

name :: Parser Name
name = do
  leading <- many $ oneOf sep
  first <- alphaNumThenUnderscore
  rest <- many1 (try alphaNumThenUnderscore <|> many1 alphaNum)
  return $ Name (init (leading ++ first ++ (concat $ init rest))) (last rest)
  where
    alphaNumThenUnderscore = (++) <$> many1 alphaNum <*> (many1 $ oneOf sep)

parseOrDie :: MonadIO m => String -> Int -> String -> m Name
parseOrDie file lineNum source = Err.parseOrDie name file lineNum source

toString :: Name -> String
toString n = _prefix n <> sep <> _hash n
