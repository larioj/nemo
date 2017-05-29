{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graph where

import Data.Set
    ( Set
    , toList
    , fromList
    , member
    , insert
    )

import Control.Monad.Trans.State
    ( State
    , runState
    )

import Util
    ( choose
    )

import StateCtl
    ( seen
    , record
    , transform
    , runStateCtl
    )

class Graph g v e | g -> v e where
    vertices :: g -> Set v
    children :: g -> v -> Set v
    merge :: g -> g -> g
    add :: v -> g -> g
    map :: g -> (e -> e) -> v -> v

dfv :: (Graph g v e) => g -> [v]
dfv g = dfv' g (toList . vertices $ g) ([], fromList [])

dfv' :: (Graph g v e) => g [v] ([v], Set v) -> ([v], Set v)
dfv' g children st = foldl iter st chidren
    where
        --iter :: ([v], Set v) -> v -> ([v], Set v)
        iter (prog, seen) v = if (v `member` seen) then (prog, seen)
            else
                let (prog', seen') = dfv' g (toList . children g $ v) (prog, insert v seen)
                in (v : prog', seen')

dfFold :: (Graph g v e, Ord v) => (a -> v -> a) -> a -> g -> a
dfFold f init g = runStateCtl (dfv' . vertices $ g) $ init
    where dfv' vs = mapM_ iter (toList vs)
          iter v = seen v >>=
                   choose (return ()) (recur v)
          recur v = record v >>= \_ ->
                    (dfv' . children g $ v) >>= \_ ->
                    transform f v

topoSort :: (Graph g v e, Ord v) => g -> [v]
topoSort = dfFold (flip (:)) []
