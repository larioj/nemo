module Nemo.Cat where

import           Control.Lens           (makeLenses, over, re, set, view, (^.),
                                         (^?))
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.RWS.Lazy (RWST, ask, gets, modify, tell)
import           Data.Foldable          (for_)
import           Data.List.Stack        (peek, pop, push)
import qualified Data.Map               as Map
import           Data.Nemo.Cat.State    (State, names, seen)
import           Data.Nemo.Directive    (Directive (Content, Export, Include),
                                         _Directive)
import           Data.Nemo.Env          (Env)
import           Data.Nemo.Error        (maybeDie)
import qualified Data.Nemo.Error        as Err
import           Data.Nemo.Extensions   (asFn, at)
import           Data.Nemo.Log          (Log)
import           Data.Nemo.Name         (Name (Name), hash, _Name)
import           Data.Nemo.NcuInfo      (NcuInfo, canonicalName, contentPath,
                                         language, name, readNcuInfo)
import qualified Data.Set               as Set

cat :: Name -> RWST Env Log State IO ()
cat parent = do
  parentInfo <- readNcuInfo parent
  modify $ over names (push Map.empty)
  modify $ set (seen . at (parentInfo ^. canonicalName)) True
  contents <- liftIO $ readFile (parentInfo ^. contentPath)
  for_ (lines contents) $ \line -> do
    directive <- maybeDie Err.BadDirective $ line ^? _Directive
    case directive of
      Include child alias -> do
        childInfo <- readNcuInfo child
        modify $ over (names . peek) (Map.insert alias child)
        seenChild <- gets $ view (seen . at (childInfo ^. canonicalName))
        unless seenChild $ cat child
      Export alias prefix -> do
        modify $ over (names . peek) $ Map.insert alias $
          Name prefix (parentInfo ^. name . hash)
      Content tokens -> do
        ns <- gets $ Map.map (^. re _Name) . view (names . peek)
        liftIO $ putStrLn $ concat $ fmap (asFn id ns) tokens
  localNames <- gets $ view (names . peek)
  tell $ [show localNames]
  modify $ over names pop
