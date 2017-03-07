module NemoLib.FlipFmap where

infixl 4 $>>
($>>) :: Functor f => f a -> (a -> b) -> f b
($>>) = flip (<$>)
