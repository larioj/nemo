module NemoLib.ShiftedBase16Hash where

import NemoLib.ShiftBase16
import           Crypto.Hash.SHA256(hash)
import           Data.ByteString.Base16(encode)
import           Data.ByteString.Char8  (pack, unpack)

shiftedBase16Hash :: String -> String
shiftedBase16Hash =
    shiftBase16 . unpack . encode . hash . pack
