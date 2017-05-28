module Util
  ( choose
  , if'
  ) where

choose :: a -> a -> Bool -> a
choose a b cond = if cond then a else b

if' :: Bool -> a -> a -> a
if' cond a b = if cond then a else b
