module Util where

import           Data.List        (intercalate)
import           Data.List.Utils  (replace)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import qualified Data.Maybe       as Maybe
import           System.Directory (doesPathExist)

choose :: a -> a -> Bool -> a
choose a b cond = if cond then a else b

if' :: Bool -> a -> a -> a
if' cond a b = if cond then a else b

putShowLn :: Show a => a -> IO ()
putShowLn = putStrLn . show

prettyPrintList :: Show a => [a] -> IO ()
prettyPrintList list =
    putStrLn $ "[  " ++ inner ++ "\n]"
    where
        inner = intercalate "\n,  " $ map show list

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond a b = cond >>= choose a b

ifM_ :: Monad m => m Bool -> m a -> m b -> m ()
ifM_ cond a b = ifM cond (a >> return ()) (b >> return ())

allBefore :: Eq a => a -> [a] -> [a]
allBefore el =
    takeWhile (/= el)

select :: (a -> Bool) -> [a] -> [a]
select = filter

replaceSafe :: Eq a => [a] -> [a] -> [a] -> [a]
replaceSafe [] _ _ = error "The first argument to replaceSafe must not be []"
replaceSafe old new cont = replace old new cont

readFileWithDefault :: String -> FilePath -> IO String
readFileWithDefault defCont path =
    ifM (doesPathExist path)
        (readFile path)
        (return defCont)

flattenMap :: Map k (Maybe v) -> Map k v
flattenMap m =
    Map.map Maybe.fromJust $
    Map.filter Maybe.isJust m
