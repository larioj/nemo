module Data.Nemo.Directive.Lens where

import Control.Lens(lens, Lens')
import Data.Nemo.Name (Name)
import Data.Nemo.Checkin (Checkin)
import qualified Data.Nemo.Directive as D
import Data.Nemo.Directive (Directive(..))

_Include :: Prism' Directive (Either Name Checkin, String)
_Include = prism (\(e, a) -> Include e a) (\d -> 
  case d of
    Include e a -> Right (e, a)
    other -> Left other)

_Export :: Prism' Directive (String, String)
_Export = prism (\(a, p) -> Export a p) (\d ->
  case d of
    Export a p -> Right (a, p)
    other -> Left other)

_Content :: Prism' Directive [String]
_Content = prism (\t -> Content t) (\d ->
  case d of
    Content t -> Right t
    other -> Left other)

expression :: Traversal' Directive (Either Name Checkin)
expression = _Include . _1

alias :: Traversal' Directive String
alias = 
