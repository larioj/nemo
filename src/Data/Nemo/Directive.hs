{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Directive where

import           Control.Lens    (Prism', makeLenses, prism', re, (^.), (^?))
import           Data.List       (stripPrefix)
import           Data.List.Split (splitOneOf)
import           Data.Nemo.Name  (Name, _Name)
import           Nemo.Clang      (tokenize)

data Directive
  = Include { _name  :: Name
            , _alias :: String }
  | Export { _alias  :: String
           , _prefix :: String }
  | Content { _tokens :: [String] }

makeLenses ''Directive

_Directive :: Prism' String Directive
_Directive = prism' toString fromString

fromString :: String -> Maybe Directive
fromString str =
  case stripPrefix ["#nemo"] $ words str of
    Just ["include", name, alias] ->
      fmap (\n -> Include n alias) (name ^? _Name)
    Just ["export", alias, prefix] -> Just $ Export alias prefix
    Nothing -> Just $ Content (tokenize str)
    Just other -> Nothing

toString :: Directive -> String
toString directive =
  case directive of
    Include name alias  -> unwords ["#nemo include", name ^. re _Name, alias]
    Export alias prefix -> unwords ["#nemo export", alias, prefix]
    Content tokens      -> concat tokens
