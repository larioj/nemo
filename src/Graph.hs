{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graph where

import qualified Data.Map as Map
import Data.Map
    ( Map
    , findWithDefault
    , keysSet
    )
import qualified Data.Set as Set
import Data.Set
    ( Set
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

type Graph k = Map k (Set k)

dfv :: Ord k => Set k -> Set k -> (a -> k -> a) -> a -> Graph k -> a
dfv seeds ctl f init g = runStateCtl (dfv' seeds) $ (init, ctl)
    where dfv' = mapM_ iter . Set.toList
          iter k = seen k >>=
                   choose (return ()) (recur k)
          recur k = record k >>= \_ ->
                    (dfv' . findWithDefault Set.empty k $ g) >>= \_ ->
                    transform f k

dfFold :: Ord k => (a -> k -> a) -> a -> Graph k -> a
dfFold f init g  = dfv (keysSet g) Set.empty f init g

topoSort :: Ord k => Graph k -> [k]
topoSort = dfFold (flip (:)) []

fromList :: Ord k => [(k, [k])] -> Graph k
fromList = Map.map Set.fromList . Map.fromList

empty :: Ord k => Graph k
empty = fromList []

toList :: Ord k => Graph k -> [(k, [k])]
toList = Map.toList . Map.map Set.toList
