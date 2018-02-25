#!/usr/bin/env runhaskell

import Control.Applicative (liftA, liftA2)
import Control.Monad.Trans.State.Lazy
import Data.Char (isAlphaNum)
import Data.Either.Combinators (mapLeft)
import Data.Function (on)
import Data.List (group, sort, foldl', intersperse)
import Data.Map (Map)
import Data.Set (Set)
import Data.Tuple (swap)
import GHC.Exts (groupWith, sortWith)
import Prelude hiding (lines)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (State, tokens)
import qualified Data.Map as Map
import qualified Data.Set as Set

usage input =
  "unrecognized command: " ++ show input `nln`
  "Usage:" `nln`
  "  nemo import local [ID1] as [ID2]" `nln`
  "    replaces all occurences of ID2 with ID1" `nln`
  "    and removes as clause from import in file $samfile"

nemo ["export", path] = undefined
nemo ["import", "local", orig, "as", alias] = let
  removeAs = toString . replaceImportLocalAs orig alias in
  (fmap . fmap) (>>= removeAs) parsedFile >>= putOrFail
nemo ["import", "local", id] = undefined
nemo input = putOrFail $ Left $ usage input

main = getArgs >>= nemo
--main = parsedFile >>= print

replaceImportLocalAs orig alias token = case token of
  unchanged@(Ident ident) ->
    if ident == alias then Ident orig else unchanged
  unchanged@(ImportLocalAs pad1 orig' pad2 alias') ->
    if alias == alias' && orig == orig'
    then ImportLocal pad1 orig' else unchanged
  unchanged -> unchanged 

data Token =
    ImportLocal [String] String
  | ImportLocalAs [String] String [String] String
  | Ident String
  | Text String
    deriving (Show, Eq)

toString (ImportLocal pad1 orig) = 
  concat $ pad1 ++ [orig]
toString (ImportLocalAs pad1 orig pad2 alias) =
  concat $ pad1 ++ [orig] ++ pad2 ++ [alias]
toString (Ident t) = t
toString (Text t) = t

parsedFile :: IO (Either String [Token])
parsedFile = samfile >>= return . (>>= parseFile)

samfile :: IO (Either String (String, String))
samfile =
  lookupEnv "samfile" >>=
  return . maybeToRight "$samfile not set" >>=
  traverse ((liftA2 . liftA2) (,) (return . id) readFile)

parseFile (name, content) = let
  lns = parse lines name content
  numbered = fmap (zip [0..]) lns
  parseNumbered (num, ln) =
    parse tokens (name ++ ": line " ++ show num) ln
  res = ((=<<) . traverse) parseNumbered numbered in
  mapLeft show $ fmap concat res

tokens =
  fmap concat $
    many1 (
      try importLocalAs <|> 
      try importLocal <|> 
      ident <|> 
      text
    )
text = fmap (return . Text) notIdentString
ident = fmap (return . Ident) identString
importLocal = do
  pad1 <- importLocalLit
  orig <- identString
  trail <- many space
  return [ImportLocal pad1 orig, Text trail]
importLocalAs = do
  pad1 <- importLocalLit
  orig <- identString
  pad2 <- sequence [many1 space, string "as", many1 space]
  alias <- identString
  trail <- many space
  return [ImportLocalAs pad1 orig pad2 alias, Text trail]
importLocalLit = 
  withSpaces [
    string "#",
    string "nemo",
    string "import",
    string "local"
  ]

isIdentChar = liftA2 (||) isAlphaNum (== '_')
identString = many1 $ satisfy isIdentChar
notIdentString = many1 $ satisfy (liftA not isIdentChar)
withSpaces ps = let
  spaces = [(many space :: Parser String)]
  middle = intersperse (many1 space :: Parser String) ps in
  sequence $ spaces ++ middle ++ spaces
lines = let
  line = many1 $ satisfy $ (/=) '\n'
  newlines = many $ char '\n'
  lineNewline = liftA2 (++) line newlines in
  many1 (lineNewline :: Parser String)

maybeToRight e Nothing = Left e
maybeToRight e (Just v) = Right v

nln a b = a ++ "\n" ++ b

putOrFail = 
  either (\msg -> putStrLn msg >> exitFailure) putStr

data Counts a =
  Counts 
    { usingZero_ :: Set a,
      usingMany_ :: Map a Int,
      allUsing_ :: Map a [a]
    } deriving (Show)

topoSort :: Ord a => Map a [a] -> Maybe [a]
topoSort g = let
  computation =
    while (not . Set.null . usingZero_)
      decrementZeros
  init = counts g
  (result, (Counts _ remainder _)) = runState computation init in
  maybeWhen (Map.null remainder) (concat result)

decrementZeros :: Ord a => State (Counts a) [a]
decrementZeros = 
  (fmap (Set.toList . usingZero_) get) >>=
  traverse decrement

decrement :: Ord a => a -> State (Counts a) a
decrement a = let
  dec b = state $ decrement' b
  allUsingA = fmap (Map.findWithDefault [] a . allUsing_) get
  toBeDec = fmap (a :) allUsingA in
  toBeDec >>= traverse dec >> return a

decrement' :: Ord a =>  a -> Counts a -> (a, Counts a)
decrement' a counts@(Counts usingZero usingMany _) = let
  isUsedByMany = Map.member a usingMany
  update =
    if isUsedByMany && (usingMany Map.! a) == 1 then let
      usingZero' = Set.insert a usingZero
      usingMany' = Map.delete a usingMany in
      counts { usingZero_ = usingZero', usingMany_ = usingMany' }
    else if isUsedByMany then let
      newCount = (usingMany Map.! a) - 1
      usingMany' = Map.insert a newCount usingMany in
      counts { usingMany_ = usingMany' }
    else let
      usingZero' = Set.delete a usingZero in
      counts { usingZero_ = usingZero' } in 
    (a, update)

invert :: Ord a => Map a [a] -> Map a [a]
invert g =
  (\f -> Map.foldlWithKey' f Map.empty g) $ \acc key items -> let
    init = ifNotMemberInsert key [] acc in
    (\f -> foldl' f init items) $ \acc item -> let
      old = Map.findWithDefault [] item acc in
      Map.insert item (key : old) acc

counts :: Ord a => Map a [a] -> Counts a
counts g = let
  all = Set.union (Map.keysSet g) (Set.fromList . concat . Map.elems $ g)
  usingMany = Map.filter (> 0) . Map.map length $ g
  usingZero = all Set.\\ (Map.keysSet usingMany)
  allUsing = invert g in
  Counts usingZero usingMany allUsing

ifNotMemberInsert :: Ord k => k -> v -> Map k v -> Map k v
ifNotMemberInsert k v m =
  if Map.member k m then m else Map.insert k v m

while :: (s -> Bool) -> State s a -> State s [a]
while cond block =
  get >>= \st ->
  if cond st then
    block >>= \val ->
    fmap (val :) $ while cond block
  else
    return []

maybeWhen :: Bool -> a -> Maybe a
maybeWhen cond value =
  if cond then Just value else Nothing

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)
