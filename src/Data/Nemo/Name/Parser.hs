module Data.Nemo.Name.Parser where

import           Control.Lens       (Prism', prism')
import           Data.Nemo.Name     (Name (Name), _hash, _prefix)
import           Nemo.Util          (silence)
import           Text.Parsec        (many, many1, oneOf, parse, try, (<|>))
import           Text.Parsec.Char   (alphaNum)
import           Text.Parsec.String (Parser)
import qualified Data.Nemo.Error as Err
import Control.Monad.IO.Class (MonadIO)

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

parseOrDie :: MonadIO m => String -> Int ->String -> m Name
parseOrDie file lineNum source = Err.parseOrDie name file lineNum source

--_Name :: Prism' String Name
--_Name = prism' toString fromString
--
--fromString :: String -> Maybe Name
--fromString = silence . parse name "LINE"
--
--toString :: Name -> String
--toString n = _prefix n <> sep <> _hash n
