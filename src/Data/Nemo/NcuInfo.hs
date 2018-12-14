{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Nemo.NcuInfo where

import           Control.Lens           (makeLenses, over, set, (^.))
import           Control.Monad          (unless)
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
import           Data.Nemo.Name         (Name (Name))
import qualified Data.Nemo.Name.Parser  as Name
import           GHC.Generics           (Generic)
import           Prelude                hiding (readFile, writeFile)
import           System.Directory       (doesPathExist)
import           System.FilePath        (joinPath)
import           System.FilePath        (combine)
import qualified System.FilePath        as P
import           System.FilePath.Lens   (basename, extension)
import           System.Nemo            (makeReadOnly)

data NcuInfo = NcuInfo
  { _name          :: Name
  , _canonicalName :: Name
  , _contentPath   :: FilePath
  , _metadataPath  :: FilePath
  , _language      :: String
  } deriving (Show, Generic)

makeLenses ''NcuInfo

instance ToJSON NcuInfo where
  toJSON = genericToJSON customOptions
  toEncoding = genericToEncoding customOptions

instance FromJSON NcuInfo where
  parseJSON = genericParseJSON customOptions

updateName :: NcuInfo -> Name -> NcuInfo
updateName info n =
  set name n .
  set (contentPath . basename) (Name.toString n) .
  set (metadataPath . basename) (Name.toString n) $
  info

makeAbsolute :: Env -> NcuInfo -> NcuInfo
makeAbsolute env =
  over contentPath (combine (env ^. sources)) .
  over metadataPath (combine (env ^. metadata))

makeRelative :: Env -> NcuInfo -> NcuInfo
makeRelative env =
  over contentPath (P.makeRelative (env ^. sources)) .
  over metadataPath (P.makeRelative (env ^. metadata))

canonicalNcuInfo :: FilePath -> String -> RWST Env Log a IO NcuInfo
canonicalNcuInfo path hash = do
  env <- ask
  let name = Name (path ^. basename) hash
  return
    NcuInfo
      { _name = name
      , _canonicalName = name
      , _contentPath = joinPath [env ^. sources, Name.toString name]
      , _metadataPath = joinPath [env ^. metadata, Name.toString name]
      , _language = path ^. extension
      }

writeNcuInfo :: NcuInfo -> RWST Env Log a IO ()
writeNcuInfo ncuInfo = do
  env <- ask
  ncuInfoExists <- liftIO $ doesPathExist (ncuInfo ^. metadataPath)
  unless ncuInfoExists $ do
    liftIO $
      writeFile (ncuInfo ^. metadataPath) $ encode (makeRelative env ncuInfo)
    makeReadOnly $ ncuInfo ^. metadataPath

readNcuInfo :: Name -> RWST Env Log a IO NcuInfo
readNcuInfo name = do
  env <- ask
  encoded <- liftIO . readFile $ joinPath [env ^. metadata, Name.toString name]
  maybeDie Err.UnableToDecodeNcuInfo $ fmap (makeAbsolute env) (decode encoded)
