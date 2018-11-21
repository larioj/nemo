module Lib where

import           Control.Applicative
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
import           System.Exit
import           System.FilePath

data Checkin =
  Checkin FilePath

data Directive
  = Include (Either String Checkin)
            String
  | Export String
           String
  | Content [String]

nemoDir :: FilePath
nemoDir = "nemolib"

getNemoPath :: IO (Maybe FilePath)
getNemoPath = getMarkerPath nemoDir

fromMaybeOrDie :: IO (Maybe a) -> IO a
fromMaybeOrDie iom = do
  m <- iom
  case m of
    Nothing -> exitFailure
    Just a  -> return a

getNemoPathOrDie :: IO FilePath
getNemoPathOrDie = fromMaybeOrDie getNemoPath

srcDir :: FilePath
srcDir = joinPath [nemoDir, "src"]

getSrcPath :: IO (Maybe FilePath)
getSrcPath = getMarkerPath srcDir

getSrcPathOrDie :: IO FilePath
getSrcPathOrDie = fromMaybeOrDie getSrcPath

parseDirective :: String -> Maybe Directive
parseDirective raw =
  let ws = words =<< splitOneOf "()" raw
  in case stripPrefix ["#nemo"] ws <|> stripPrefix ["#", "nemo"] ws of
        Just ["include", "checkin", path, alias] ->
          Just $ Include (Right (Checkin path)) alias
        Just ["include", name, alias] -> Just $ Include (Left name) alias
        Just ["export", alias, namePrefix] -> Just $ Export alias namePrefix
        Just other -> Nothing
        Nothing -> Just $ Content (tokenize raw)

parseDirectiveOrDie :: String -> IO Directive
parseDirectiveOrDie raw =
  case parseDirective raw of
    Just d  -> return d
    Nothing -> die $ unwords ["unable to parse:", raw]

showDirective :: Directive -> String
showDirective d =
  case d of
    Include (Left name) alias -> unwords ["#nemo include", name, alias]
    Include (Right (Checkin path)) alias ->
      concat ["#nemo include (checkin ", path, ") ", alias]
    Export alias namePrefix -> unwords ["#nemo export", alias, namePrefix]
    Content tokens -> concat tokens

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
