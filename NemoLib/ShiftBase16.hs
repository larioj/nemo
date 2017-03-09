module NemoLib.ShiftBase16 where

import NemoLib.ShiftBase16Char

shiftBase16 :: String -> String
shiftBase16 = map shiftBase16Char