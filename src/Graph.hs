{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Graph where

import qualified Data.Map as Map
import Data.Map
    ( Map
    , findWithDefault
    , foldWithKey
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
import Data.List
    ( union
    )

type Graph k = Map k (Set k)

dfv :: Ord k => Set k -> Set k -> (a -> k -> a) -> a -> Graph k -> a
dfv seeds ctl f init g = runStateCtl (dfv' seeds) $ (init, ctl)
    where
        dfv' = mapM_ iter . Set.toList
        iter k =
            seen k >>=
            choose (return ()) (recur k)
        recur k =
            record k >>
            (dfv' . findWithDefault Set.empty k $ g) >>
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

inverse :: Ord k => Graph k -> Graph k
inverse g = foldWithKey invert init g
    where
        keys = Set.toList . Map.keysSet $ g
        values = concat . map Set.toList . Map.elems $ g
        elems = union keys values
        init = foldl (\g k -> Map.insert k Set.empty g) Map.empty elems
        invert v sk g = foldl (insert v) g (Set.toList sk)
        insert v g k = Map.insert k newv g
            where
                oldv = findWithDefault Set.empty k g
                newv = Set.insert v oldv
