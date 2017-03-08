#!/usr/bin/env runhaskell

import           Crypto.Hash.SHA256
import           Data.ByteString.Base16
import           Data.ByteString.Char8  (pack, unpack)
import           Data.Char
import           Data.List              ((\\))
import           Data.Maybe
import           NemoLib.Delete
import           NemoLib.File
import           NemoLib.FlipFmap
import           NemoLib.If
import           NemoLib.NemoNode
import           NemoLib.Select
import           NemoLib.ShadowNode
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

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
nemo _                   = putStrLn "error: wrong usage"

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
    deleteNemoFileAndNemoShadow >>=
    getNemoPointers

deleteNemoFileAndNemoShadow :: [FilePath] -> [FilePath]
deleteNemoFileAndNemoShadow =
    (delete nemoShadowName) . (delete nemoFileName)

getNemoPointers :: [FilePath] -> IO [(String, FilePath)]
getNemoPointers = mapM getNemoPointer

getNemoPointer :: FilePath -> IO (String, FilePath)
getNemoPointer fp =
    getNemoLeafPointer fp $>> \p ->
    (p ++ "_" ++ fp, fp)

getNemoNodePointer :: FilePath -> IO String
getNemoNodePointer = undefined

getNemoLeafPointer :: FilePath -> IO String
getNemoLeafPointer fp =
    readFile (nemoDirectoryPath </> fp) $>>
    (shiftBase16 . unpack . encode . hash . pack)

showStateDifferences :: [(String, FilePath)] -> [(String, FilePath)] -> String
showStateDifferences nemo domain =
    (show nemo) ++ "\n" ++ (show domain)

shiftBase16 :: String -> String
shiftBase16 = map shiftBase16Char

shiftBase16Char :: Char -> Char
shiftBase16Char c =
    (chr . ((+) x) . ord) c
    where x = if' (ord c >= ord '0' && ord c <= ord '9')
                  ((ord 'A') - (ord '0'))
                  ((ord 'K') - (ord 'a'))

shadeFiles :: [File] -> [File]
shadeFiles = undefined

shadeNemoNodes :: [NemoNode] -> [NemoNode]
shadeNemoNodes nodes =
    foldl accumulateShadowNodes [] ((reverse . topoSort) nodes)

accumulateShadowNodes :: [NemoNode] -> NemoNode -> [NemoNode]
accumulateShadowNodes shadowNodes node =
    undefined

nemoNodeToShadowNode :: [ShadowNode] -> NemoNode -> ShadowNode
nemoNodeToShadowNode dependencies node = undefined

topoSort :: [NemoNode] -> [NemoNode]
topoSort graph =
    foldl (accumulatePath graph []) [] graph

dfsPath :: [NemoNode] -> [NemoNode] -> NemoNode -> [NemoNode]
dfsPath graph seen node
    | node `elem` seen = []
    | otherwise = node : (foldl (accumulatePath graph (node : seen)) [] (getDependencies graph node))

accumulatePath :: [NemoNode] -> [NemoNode] -> [NemoNode] -> NemoNode -> [NemoNode]
accumulatePath graph seen path node =
    if' (node `elem` allSeen) path
        ((dfsPath graph allSeen node) ++ path)
    where allSeen = seen ++ path

getDependencies :: [NemoNode] -> NemoNode -> [NemoNode]
getDependencies graph node =
    (catMaybes . (map (lookupNemoNode graph))) (dependencies node)

lookupNemoNode :: [NemoNode] -> String -> Maybe NemoNode
lookupNemoNode graph path = lookup path (associate graph)

associate :: [NemoNode] -> [(String, NemoNode)]
associate = map (\n  -> (address n, n))
