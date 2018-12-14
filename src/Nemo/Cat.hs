module Nemo.Cat where

import           Control.Lens           (over, set, view, (^.), (^?))
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.RWS.Lazy (RWST, gets, modify, tell)
import           Control.Nemo           (runState)
import           Data.Foldable          (for_)
import           Data.List.Stack        (peek, pop, push)
import qualified Data.Map               as Map
import           Data.Nemo.Cat.State    (State (State), names, seen)
import           Data.Nemo.Directive    (Directive (Content, Export, Include))
import           Data.Nemo.Env          (Env)
import           Data.Nemo.Error        (maybeDie)
import qualified Data.Nemo.Error        as Err
import           Data.Nemo.Extensions   (asFn, at)
import           Data.Nemo.Log          (Log)
import           Data.Nemo.Name         (Name (Name), hash)
import           Data.Nemo.NcuInfo      (canonicalName, contentPath, name,
                                         readNcuInfo)
import qualified Data.Set               as Set
import           Parser.Nemo.Directive  (_Directive)
import qualified Parser.Nemo.Name       as Name

cat :: FilePath -> RWST Env Log a IO ()
cat path = runState (State [] Set.empty) $ inlineDirectives path Nothing

catName :: Name -> RWST Env Log a IO ()
catName = runState (State [] Set.empty) . cat'

cat' :: Name -> RWST Env Log State IO ()
cat' parent = do
  parentInfo <- readNcuInfo parent
  modify $ over names (push Map.empty)
  modify $ set (seen . at (parentInfo ^. canonicalName)) True
  inlineDirectives
    (parentInfo ^. contentPath)
    (Just $ parentInfo ^. name . hash)
  localNames <- gets $ view (names . peek)
  tell $ [show localNames]
  modify $ over names pop

inlineDirectives :: String -> Maybe String -> RWST Env Log State IO ()
inlineDirectives path contentHash = do
  contents <- liftIO $ readFile path
  for_ (lines contents) $ \line -> do
    directive <- maybeDie Err.BadDirective $ line ^? _Directive
    case directive of
      Include child alias -> do
        childInfo <- readNcuInfo child
        modify $ over (names . peek) (Map.insert alias child)
        seenChild <- gets $ view (seen . at (childInfo ^. canonicalName))
        unless seenChild $ cat' child
      Export alias prefix ->
        case contentHash of
          Nothing -> return ()
          Just h ->
            modify $ over (names . peek) $ Map.insert alias $ Name prefix h
      Content tokens -> do
        ns <- gets $ Map.map (Name.toString) . view (names . peek)
        liftIO $ putStrLn $ concat $ fmap (asFn id ns) tokens
