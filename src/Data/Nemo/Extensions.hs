{-# LANGUAGE Rank2Types #-}

module Data.Nemo.Extensions where

import           Control.Lens (Lens', lens)
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Set     (Set, delete, insert, member)

at :: Ord a => a -> Lens' (Set a) Bool
at a =
  lens
    (member a)
    (\s shouldInsert ->
       if shouldInsert
         then insert a s
         else delete a s)

asFn :: Ord k => (k -> v) -> Map k v -> k -> v
asFn fallback m k =
  if M.member k m
    then m M.! k
    else fallback k
