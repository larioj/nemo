module Data.Nemo.Directive where

import Data.Nemo.Name (Name)
import Data.Nemo.Checkin (Checkin)

data Directive
  = Include
      { expression :: Either Name Checkin
      , alias :: String
      }
  | Export
      { alias :: String
      , prefix :: String
      }
  | Content
      { tokens :: [String]
      }
