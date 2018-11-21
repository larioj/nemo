module Data.Nemo.Checkin.Lens where

import Data.Nemo.Checkin (Checkin)
import qualified Data.Nemo.Checkin as C
import Control.Lens (lens, Lens')

path :: Lens' Checkin FilePath
path = lens C.path (\c p -> c { C.path = p })
