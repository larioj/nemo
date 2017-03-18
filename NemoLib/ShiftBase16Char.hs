module NemoLib.ShiftBase16Char where

import           Data.Char  (Char, chr, ord)
import           NemoLib.If

shiftBase16Char :: Char -> Char
shiftBase16Char c =
    (chr . ((+) x) . ord) c
    where x = if' (ord c >= ord '0' && ord c <= ord '9')
                  ((ord 'A') - (ord '0'))
                  ((ord 'K') - (ord 'a'))
