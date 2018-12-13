{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Nemo.Name where

import           Control.Lens    (makeLenses)
import           Data.Aeson      (FromJSON, ToJSON, genericParseJSON,
                                  genericToEncoding, genericToJSON, parseJSON,
                                  toEncoding, toJSON)
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
