module NemoLib.ShiftedBase16Hash where

import           Crypto.Hash.SHA256     (hash)
import           Data.ByteString.Base16 (encode)
import           Data.ByteString.Char8  (pack, unpack)
import           NemoLib.ShiftBase16

shiftedBase16Hash :: String -> String
shiftedBase16Hash =
    shiftBase16 . unpack . encode . hash . pack
