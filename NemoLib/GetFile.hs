module NemoLib.GetFile where

import NemoLib.File
import NemoLib.FlipFmap

getFile :: FilePath -> IO File
getFile path =
    readFile path $>>
    File path
