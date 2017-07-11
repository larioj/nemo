module Graph where

import           Control.Monad (forM_)
import           Data.List     (union)
import           Data.Map      (Map, findWithDefault, foldWithKey, keysSet)
import qualified Data.Map      as Map
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Prelude       hiding (lookup)
import           StateCtl      (record, runStateCtl, seen, transform)
import           Util          (ifM_)

type Graph k = Map k (Set k)

dfv :: Ord k => Set k -> Set k -> (a -> k -> a) -> a -> Graph k -> a
dfv seeds ctl f init g = runStateCtl (dfv' seeds) $ (init, ctl)
    where
        dfv' ks =
            forM_ (Set.toList ks) (\k ->
                ifM_ (seen k) (return ()) (
                    record k >>
                    (dfv' (lookup k  g)) >>
                    transform f k
                )
            )

dfFold :: Ord k => (a -> k -> a) -> a -> Graph k -> a
dfFold f init g  = dfv (keysSet g) Set.empty f init g

topoSort :: Ord k => Graph k -> [k]
topoSort = dfFold (flip (:)) []

graph :: Ord k => [(k, [k])] -> Graph k
graph = Map.map Set.fromList . Map.fromList

empty :: Ord k => Graph k
empty = graph []

toList :: Ord k => Graph k -> [(k, [k])]
toList = Map.toList . Map.map Set.toList

invert :: Ord k => Graph k -> Graph k
invert g = foldWithKey invertOne init g
    where
        keys = Set.toList . Map.keysSet $ g
        values = concat . map Set.toList . Map.elems $ g
        elems = union keys values
        init = Map.fromList . map (\k -> (k, Set.empty)) $ elems
        invertOne v sk g = foldl (insert v) g (Set.toList sk)
        insert v g k = Map.insert k newv g
            where
                oldv = findWithDefault Set.empty k g
                newv = Set.insert v oldv

successors :: Ord k => Graph k -> k -> Set k
successors g k = Set.fromList $ dfv (Set.singleton k) Set.empty (flip (:)) [] g

lookup :: Ord k => k -> Graph k -> Set k
lookup k g = findWithDefault Set.empty k g

isSuccessor :: Ord k => Graph k -> k -> k -> Bool
isSuccessor g possibleSuccessor parent
    | (parent == possibleSuccessor) = True
    | otherwise =
        any (isSuccessor g possibleSuccessor) (Graph.lookup parent g)
