module Lib where

import System.FilePath
import System.Directory
import System.Environment
import Data.Foldable
import Data.Char
import Data.List
import Data.Maybe
import Data.Traversable
import Data.List.Split

data Directive 
  = Include String String
  | Export String
  | Content [String]

parseDirective :: String -> Directive
parseDirective raw
  | "#nemo include " `isPrefixOf` raw = let 
    [_, _, name, alias] = words raw in
    Include name alias
  | "#nemo export " `isPrefixOf` raw = let
    [_, _, alias] = words raw in
    Export alias
  | otherwise =
    Content (tokenize raw)

paths :: FilePath -> [FilePath]
paths = reverse . tail . map concat . inits . splitPath 

isJustIf :: a -> Bool -> Maybe a
isJustIf v cond = 
  if cond then Just v else Nothing

getMarkerPath :: String -> IO (Maybe FilePath)
getMarkerPath marker = do
  cwd <- getCurrentDirectory
  markerPaths <- 
    for (paths cwd) $ \p -> let
      mp = joinPath [p, marker] in
      fmap (isJustIf mp) (doesPathExist mp)
  return $ msum markerPaths

touchDirectory :: FilePath -> IO ()
touchDirectory path = do
  exists <- doesDirectoryExist path
  if exists then
    return () else
      createDirectory path

at :: [a] -> Int -> Maybe a
at list idx =
  if idx >= length list || idx < 0  then 
    Nothing else 
    Just $ list !! idx

tokenize :: String -> [String]
tokenize raw =
  groupBy sameTokenClass raw

sameTokenClass :: Char -> Char -> Bool
sameTokenClass a b =
  (isSpace a && isSpace b) ||
  (isIdentifier a && isIdentifier b) ||
  (isOperator a && isOperator b)

isIdentifier :: Char -> Bool
isIdentifier '_' = True
isIdentifier c = isAlphaNum c

isOperator :: Char -> Bool
isOperator c =
  not (isSpace c || isIdentifier c)
