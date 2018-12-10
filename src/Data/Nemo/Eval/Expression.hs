{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Eval.Expression where

import           Control.Lens        (makeLenses, (^.))
import           Data.Nemo.Directive (Directive (Include))
import           Data.Nemo.Name      (Name)

data Expression
  = Copy { _target :: FilePath
         , _alias  :: String }
  | Move { _target :: FilePath
         , _alias  :: String }
  deriving (Show, Eq)

makeLenses ''Expression

toDirective :: Name -> Expression -> Directive
toDirective name exp = Include name (exp ^. alias)
