{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Directive where

import           Control.Applicative ((<|>))
import           Control.Lens        (Prism', makeLenses, prism', re, (^.),
                                      (^?))
import           Data.List           (stripPrefix)
import           Data.List.Split     (splitOneOf)
import           Data.Nemo.Checkin   (Checkin (..))
import           Data.Nemo.Name      (Name, _Name)
import           Nemo.Clang          (tokenize)

data Directive
  = Include { _expression :: Either Name Checkin
            , _alias      :: String }
  | Export { _alias  :: String
           , _prefix :: String }
  | Content { _tokens :: [String] }

makeLenses ''Directive

_Directive :: Prism' String Directive
_Directive = prism' string directive

directive :: String -> Maybe Directive
directive raw =
  let ws = words =<< splitOneOf "()" raw
   in case stripPrefix ["#nemo"] ws <|> stripPrefix ["#", "nemo"] ws of
        Just ["include", "checkin", path, alias] ->
          Just $ Include (Right (Checkin path)) alias
        Just ["include", name, alias] ->
          case name ^? _Name of
            Just name -> Just $ Include (Left name) alias
            Nothing   -> Nothing
        Just ["export", alias, namePrefix] -> Just $ Export alias namePrefix
        Just other -> Nothing
        Nothing -> Just $ Content (tokenize raw)

string :: Directive -> String
string d =
  case d of
    Include (Left name) alias ->
      unwords ["#nemo include", name ^. re _Name, alias]
    Include (Right (Checkin path)) alias ->
      concat ["#nemo include (checkin ", path, ") ", alias]
    Export alias namePrefix -> unwords ["#nemo export", alias, namePrefix]
    Content tokens -> concat tokens
