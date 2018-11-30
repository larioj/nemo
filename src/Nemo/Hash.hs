module Nemo.Hash where

import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base64 as Base64
import           Data.ByteString.Char8  (ByteString, pack, unpack)

base64ToAlpha :: Char -> Char
base64ToAlpha c =
  case c of
    '+' -> 'p'
    '/' -> 's'
    '=' -> 'e'
    c   -> c

encode :: ByteString -> String
encode = map base64ToAlpha . unpack . Base64.encode
