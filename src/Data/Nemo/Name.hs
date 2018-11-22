{-# LANGUAGE TemplateHaskell #-}

module Data.Nemo.Name where

import           Control.Lens    (Prism', makeLenses, prism', (^?), _Snoc)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

data Name = Name
  { _prefix :: String
  , _hash   :: String
  } deriving (Show, Eq, Ord)

makeLenses ''Name

sep :: String
sep = "_"

_Name :: Prism' String Name
_Name = prism' string name

name :: String -> Maybe Name
name s = do
  (rest, hash) <- splitOn sep s ^? _Snoc
  prefix <- return $ intercalate sep rest
  if null prefix || null hash
    then Nothing
    else Just $ Name prefix hash

string :: Name -> String
string n = _prefix n <> sep <> _hash n
