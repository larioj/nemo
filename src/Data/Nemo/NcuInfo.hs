{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Nemo.NcuInfo where

import           Control.Lens           (makeLenses, over, re, (^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.RWS.Lazy (RWST, ask)
import           Data.Aeson             (FromJSON, ToJSON, decode, encode,
                                         genericParseJSON, genericToEncoding,
                                         genericToJSON, parseJSON, toEncoding,
                                         toJSON)
import           Data.ByteString.Lazy   (readFile, writeFile)
import           Data.Nemo.Codec        (customOptions)
import           Data.Nemo.Env          (Env, metadata, sources)
import           Data.Nemo.Error        (maybeDie)
import qualified Data.Nemo.Error        as Err
import           Data.Nemo.Log          (Log)
import           Data.Nemo.Name         (Name, _Name)
import           GHC.Generics           (Generic)
import           Prelude                hiding (readFile, writeFile)
import           System.FilePath        (joinPath)
import           System.FilePath        (combine)
import qualified System.FilePath        as P

data NcuInfo = NcuInfo
  { _name          :: Name
  , _canonicalName :: Name
  , _contentPath   :: FilePath
  , _language      :: String
  } deriving (Show, Generic)

makeLenses ''NcuInfo

instance ToJSON NcuInfo where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON NcuInfo where
  parseJSON = genericParseJSON customOptions

makeAbsolute :: Env -> NcuInfo -> NcuInfo
makeAbsolute env = over contentPath (combine (env ^. sources))

makeRelative :: Env -> NcuInfo -> NcuInfo
makeRelative env = over contentPath (P.makeRelative (env ^. sources))

writeNcuInfo :: NcuInfo -> RWST Env Log a IO ()
writeNcuInfo ncuInfo = do
  env <- ask
  liftIO $
    writeFile (ncuInfo ^. contentPath) $ encode (makeRelative env ncuInfo)

readNcuInfo :: Name -> RWST Env Log a IO NcuInfo
readNcuInfo name = do
  env <- ask
  encoded <- liftIO . readFile $ joinPath [env ^. metadata, name ^. re _Name]
  maybeDie Err.UnableToDecodeNcuInfo $ fmap (makeAbsolute env) (decode encoded)
