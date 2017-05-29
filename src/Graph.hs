{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graph where

import Data.Set
    ( Set
    , toList
    , fromList
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

dfFold :: (Graph g v e, Ord v) => (a -> v -> a) -> a -> g -> a
dfFold f init g = runStateCtl (dfv' . vertices $ g) $ init
    where dfv' = mapM_ iter . toList
          iter v = seen v >>=
                   choose (return ()) (recur v)
          recur v = record v >>= \_ ->
                    (dfv' . children g $ v) >>= \_ ->
                    transform f v

topoSort :: (Graph g v e, Ord v) => g -> [v]
topoSort = dfFold (flip (:)) []
