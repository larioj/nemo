module NemoLib.Delete where

import NemoLib.Select

delete :: Eq a => a -> [a] -> [a]
delete x = select ((/=) x)