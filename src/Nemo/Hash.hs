module Nemo.Hash where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.ByteString.Base64 (encode)
import           Data.ByteString.Char8  (pack, unpack)

base64ToAlpha :: Char -> Char
base64ToAlpha c =
  case c of
    '+' -> 'p'
    '/' -> 's'
    '=' -> 'e'
    c   -> c

hash :: String -> String
hash = map base64ToAlpha . unpack . encode . SHA256.hash . pack
