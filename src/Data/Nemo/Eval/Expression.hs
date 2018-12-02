{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Eval.Expression where

import           Control.Lens        (Prism', makeLenses, prism', (^.))
import           Data.List           (stripPrefix)
import           Data.List.Split     (splitOneOf)
import           Data.Nemo.Directive (Directive (Include))
import           Data.Nemo.Name      (Name)

data Expression
  = Copy { _target :: FilePath
         , _alias  :: String }
  | Move { _target :: FilePath
         , _alias  :: String }

makeLenses ''Expression

fromString :: String -> Maybe Expression
fromString str = do
  rest <- stripPrefix ["#nemo"] $ words =<< splitOneOf "()" str
  case rest of
    ["include", "copy", target, alias] -> Just $ Copy target alias
    ["include", "move", target, alias] -> Just $ Move target alias
    _                                  -> Nothing

toDirective :: Name -> Expression -> Directive
toDirective name exp = Include name (exp ^. alias)
