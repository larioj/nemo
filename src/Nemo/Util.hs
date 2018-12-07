module Nemo.Util where

import           Control.Monad    (Monad, MonadPlus, mzero)
import           Data.List        (inits)
import           System.Directory (doesPathExist, getCurrentDirectory)
import           System.FilePath  (combine, splitPath)

paths :: FilePath -> [FilePath]
paths = reverse . tail . map concat . inits . splitPath

find :: (MonadPlus mp, Eq (mp b)) => (a -> mp b) -> [a] -> mp b
find _ [] = mzero
find f (a:rest) =
  let b = f a
   in if b == mzero
        then find f rest
        else b

findM ::
     (Monad m, MonadPlus mp, Eq (mp b)) => (a -> m (mp b)) -> [a] -> m (mp b)
findM _ [] = return mzero
findM f (a:rest) =
  f a >>= \b ->
    if b == mzero
      then findM f rest
      else return b

match :: (MonadPlus mp) => (a -> Bool) -> a -> mp a
match f a =
  if f a
    then return a
    else mzero

liftInner :: Functor f => ((a -> b) -> a -> c) -> (a -> f b) -> a -> f c
liftInner trans f a = fmap (\b -> trans (const b) a) (f a)

findMarker :: String -> IO (Maybe FilePath)
findMarker marker = do
  cwd <- getCurrentDirectory
  let possibleMarkers = combine <$> paths cwd <*> pure marker
  findM (liftInner match doesPathExist) possibleMarkers

silence :: Either a b -> Maybe b
silence (Right a) = Just a
silence (Left _)  = Nothing
