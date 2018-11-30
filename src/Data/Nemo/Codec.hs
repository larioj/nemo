module Data.Nemo.Codec where

import           Data.Aeson (defaultOptions, fieldLabelModifier)
import           Data.List  (isPrefixOf)

dropLeadingUnderscore :: String -> String
dropLeadingUnderscore s =
  if isPrefixOf "_" s
    then tail s
    else s

customOptions = defaultOptions {fieldLabelModifier = dropLeadingUnderscore}
