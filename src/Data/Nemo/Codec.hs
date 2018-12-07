module Data.Nemo.Codec where

import           Data.Aeson (Options, defaultOptions, fieldLabelModifier)
import           Data.List  (isPrefixOf)

dropLeadingUnderscore :: String -> String
dropLeadingUnderscore s =
  if isPrefixOf "_" s
    then tail s
    else s

customOptions :: Options
customOptions = defaultOptions {fieldLabelModifier = dropLeadingUnderscore}
