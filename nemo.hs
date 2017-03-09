#!/usr/bin/env runhaskell

import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import NemoLib.FlipFmap
import Data.List(delete)

main :: IO ()
main = getArgs >>= nemo

nemoDirectoryPath = "NemoLib"
nemoShadowName = "ShadowLib"
nemoShadowPath = nemoDirectoryPath </> nemoShadowName
nemoFileName = ".nemo"
nemoFilePath = nemoDirectoryPath </> nemoFileName

nemo :: [String] -> IO ()
nemo ("initialize" : []) = initialize
nemo ("status" : [])     = status
nemo ("add" : file : []) = add file
nemo _                   = status

initialize :: IO ()
initialize =
    createDirectory nemoDirectoryPath >>
    createDirectory nemoShadowPath >>
    writeFile nemoFilePath "[]"

status :: IO ()
status =
    getNemoState >>= \nemoState ->
    getDomainState >>= \domainState ->
    putStrLn (showStateDifferences nemoState domainState)

add :: FilePath -> IO ()
add = undefined

getNemoState :: IO [(String, FilePath)]
getNemoState = readFile nemoFilePath $>> read

getDomainState :: IO [(String, FilePath)]
getDomainState =
    listDirectory nemoDirectoryPath $>>
    removeNemoFileAndNemoShadow $>>
    map (\d -> (d, d))

removeNemoFileAndNemoShadow :: [FilePath] -> [FilePath]
removeNemoFileAndNemoShadow =
    (delete nemoShadowName) . (delete nemoFileName)

showStateDifferences :: [(String, FilePath)] -> [(String, FilePath)] -> String
showStateDifferences nemo domain =
    (show nemo) ++ "\n" ++ (show domain)
