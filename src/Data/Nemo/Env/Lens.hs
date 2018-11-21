module Data.Nemo.Env.Lens where

import qualified Data.Nemo.Env as E
import Data.Nemo.Env (Env)
import Control.Lens (lens, Lens')

base :: Lens' Env FilePath
base = lens E.base (\e p -> e { E.base = p })

sources :: Lens' Env FilePath
sources = lens E.sources (\e p -> e { E.sources = p })
