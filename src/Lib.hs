module Lib where

import           Control.Monad
import           Crypto.Hash.SHA256     (hash)
import           Data.ByteString.Base64 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Traversable
import           System.Directory
import           System.Environment
import           System.FilePath

data Nrl
  = FsPath FilePath
  | NemoId String

data Directive
  = Include String
            String
  | Export String
  | Content [String]

parseDirective :: String -> Directive
parseDirective raw
  | "#nemo include " `isPrefixOf` raw =
    let [_, _, name, alias] = words raw
     in Include name alias
  | "#nemo export " `isPrefixOf` raw =
    let [_, _, alias] = words raw
     in Export alias
  | otherwise = Content (tokenize raw)

paths :: FilePath -> [FilePath]
paths = reverse . tail . map concat . inits . splitPath

isJustIf :: a -> Bool -> Maybe a
isJustIf v cond =
  if cond
    then Just v
    else Nothing

getMarkerPath :: String -> IO (Maybe FilePath)
getMarkerPath marker = do
  cwd <- getCurrentDirectory
  markerPaths <-
    for (paths cwd) $ \p ->
      let mp = joinPath [p, marker]
       in fmap (isJustIf mp) (doesPathExist mp)
  return $ msum markerPaths

touchDirectory :: FilePath -> IO ()
touchDirectory path = do
  exists <- doesDirectoryExist path
  unless exists $ createDirectory path

at :: [a] -> Int -> Maybe a
at list idx =
  if idx >= length list || idx < 0
    then Nothing
    else Just $ list !! idx

tokenize :: String -> [String]
tokenize = groupBy sameTokenClass

sameTokenClass :: Char -> Char -> Bool
sameTokenClass a b =
  (isSpace a && isSpace b) ||
  (isIdentifier a && isIdentifier b) || (isOperator a && isOperator b)

isIdentifier :: Char -> Bool
isIdentifier '_' = True
isIdentifier c   = isAlphaNum c

isOperator :: Char -> Bool
isOperator c = not (isSpace c || isIdentifier c)

headMaybe :: [a] -> Maybe a
headMaybe list =
  if null list
    then Nothing
    else Just $ head list

base64ToAlpha :: Char -> Char
base64ToAlpha c =
  case c of
    '+' -> 'p'
    '/' -> 's'
    '=' -> 'e'
    c   -> c

alphaHash :: String -> String
alphaHash = map base64ToAlpha . unpack . encode . hash . pack
