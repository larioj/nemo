module Data.Nemo.Name.Lens where

import Control.Lens (Lens', lens)
import qualified Data.Nemo.Name as N
import Data.Nemo.Name (Name)

prefix :: Lens' Name String
prefix = lens N.prefix (\n p -> n { N.prefix = p })

hash :: Lens' Name String
hash = lens N.hash (\n h -> n { N.hash = h })
