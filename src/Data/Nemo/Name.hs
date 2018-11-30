{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Nemo.Name where

import           Control.Lens    (Prism', makeLenses, prism', (^?), _Snoc)
import           Data.Aeson      (FromJSON, ToJSON, genericParseJSON,
                                  genericToEncoding, genericToJSON, parseJSON,
                                  toEncoding, toJSON)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import           Data.Nemo.Codec (customOptions)
import           GHC.Generics    (Generic)

data Name = Name
  { _prefix :: String
  , _hash   :: String
  } deriving (Show, Eq, Ord, Generic)

makeLenses ''Name

instance ToJSON Name where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON Name where
  parseJSON = genericParseJSON customOptions

sep :: String
sep = "_"

_Name :: Prism' String Name
_Name = prism' toString fromString

fromString :: String -> Maybe Name
fromString s = do
  (rest, hash) <- splitOn sep s ^? _Snoc
  prefix <- return $ intercalate sep rest
  if null prefix || null hash
    then Nothing
    else Just $ Name prefix hash

toString :: Name -> String
toString n = _prefix n <> sep <> _hash n
