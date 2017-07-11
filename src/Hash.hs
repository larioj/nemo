module Hash where

import           Crypto.Hash.SHA256     (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Char              (Char, chr, ord)
import           Util

base16ToAlpha :: Char -> Char
base16ToAlpha c =
    (chr . ((+) x) . ord) c
    where x = if' (ord c >= ord '0' && ord c <= ord '9')
                  ((ord 'A') - (ord '0'))
                  ((ord 'K') - (ord 'a'))

base16AlphaHash :: String -> String
base16AlphaHash =
    (map base16ToAlpha) . unpack . encode . hash . pack
