module Stack where

import Graph hiding (dfv)
import Data.Set
import Control.Monad.State

choose :: a -> a -> Bool -> a
choose a b cond = if cond then a else b

push :: a -> State ([a], Set a) ()
push a = state $ \(prog, seen) -> ((), (a : prog, seen))

record :: (Ord a) => a -> State ([a], Set a) ()
record a = state $ \(prog, seen) -> ((), (prog, insert a seen))

contains :: (Ord a) => a -> State ([a], Set a) Bool
contains a = state $ \(prog, seen) -> (member a seen, (prog, seen))

dfv :: (Graph g v e, Ord v) => g -> [v]
dfv g = fst . snd . runState (dfv' . vertices $ g) $ ([], fromList [])
    where dfv' = mapM_ iter . toList
          iter v = contains v >>= \seen ->
              if seen then return ()
                  else record v >>= \_ ->
                       (dfv' . children g $ v) >>= \_ ->
                       push v
