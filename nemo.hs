#!/usr/bin/env runhaskell

import           Data.List                      (delete, intercalate)
import           Data.Tuple                     (swap)
import           NemoLib.File
import           NemoLib.FileToNemoNode
import           NemoLib.FlipFmap
import           NemoLib.GetClosure
import           NemoLib.GetFile
import           NemoLib.GetModifiedFiles
import           NemoLib.GetNewFiles
import           NemoLib.If
import           NemoLib.NemoNode
import           NemoLib.NemoNodesToShadowNodes
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

getNemoState :: IO [ShadowNode]
getNemoState = readNemoFile >>=
               mapM getShadowNode

readNemoFile :: IO [(String, String)]
readNemoFile = readFile nemoFilePath $>> read

getShadowNode :: (String, String) -> IO ShadowNode
getShadowNode (shadow, nemo) =
    readFile (getShadowPath shadow) $>>
    ShadowNode shadow nemo

getShadowPath :: String -> FilePath
getShadowPath shadow =
    nemoShadowPath ++ shadow ++ ".hs"

getDomainState :: IO [ShadowNode]
getDomainState =
    getNemoNodes $>>
    nemoNodesToShadowNodes

removeNemoFileAndNemoShadow :: [FilePath] -> [FilePath]
removeNemoFileAndNemoShadow =
    (delete nemoShadowName) . (delete nemoFileName)

showStateDifferences :: [ShadowNode] -> [ShadowNode] -> String
showStateDifferences nemo domain =
    newFiles ++ "\n" ++ modifiedFiles
    where newFiles = formatNewFiles (getNewFiles nemo domain)
          modifiedFiles = formatModifiedFiles (getModifiedFiles nemo domain)

formatModifiedFiles :: [String] -> String
formatModifiedFiles =
    formatFiles "new"

formatNewFiles :: [String] -> String
formatNewFiles =
    formatFiles "new"

formatFiles :: String -> [String] -> String
formatFiles message files =
    intercalate "\n" $
    map (\f -> message ++ " " ++ (nemoDirectoryPath </> f) ++ ".hs") files

add :: FilePath -> IO ()
add file =
    getNemoNodes $>>
    getClosure (takeBaseName file) $>>
    nemoNodesToShadowNodes >>=
    writeShadowNodes

writeShadowNodes :: [ShadowNode] -> IO ()
writeShadowNodes = mapM_ writeShadowNode

getNemoFiles :: IO [File]
getNemoFiles =
    listDirectory nemoDirectoryPath $>>
    removeNemoFileAndNemoShadow $>>
    (map ((</>) nemoDirectoryPath)) >>=
    (mapM getFile)

writeShadowNode :: ShadowNode -> IO ()
writeShadowNode (ShadowNode hash name contents) =
    writeFileIfNotExists path contents
    where path = nemoShadowPath </> (hash ++ "_" ++ name ++ ".hs")

writeFileIfNotExists :: FilePath -> String -> IO ()
writeFileIfNotExists path contents =
    doesPathExist path >>= \exists ->
    if' exists
        (return ())
        (writeFile path contents >> (putStrLn $ "writing shadow node to " ++ path))

getNemoNodes :: IO [NemoNode]
getNemoNodes = getNemoFiles $>> map fileToNemoNode
