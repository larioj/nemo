module Util
  ( choose
  , if'
  , putShowLn
  ) where

choose :: a -> a -> Bool -> a
choose a b cond = if cond then a else b

if' :: Bool -> a -> a -> a
if' cond a b = if cond then a else b

putShowLn :: Show a => a -> IO ()
putShowLn = putStrLn . show
