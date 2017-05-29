{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graph where

import Data.Map
    ( Map
    , findWithDefault
    , keysSet
    )
import Data.Set
    ( Set
    , toList
    , fromList
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
    where dfv' = mapM_ iter . toList
          iter k = seen k >>=
                   choose (return ()) (recur k)
          recur k = record k >>= \_ ->
                    (dfv' . findWithDefault (fromList []) k $ g) >>= \_ ->
                    transform f k

dfFold :: Ord k => (a -> k -> a) -> a -> Graph k -> a
dfFold f init g  = dfv (keysSet g) (fromList []) f init g

topoSort :: Ord k => Graph k -> [k]
topoSort = dfFold (flip (:)) []
