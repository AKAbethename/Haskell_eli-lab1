import System.Environment
import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BC

import Lib

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let byteLength = BC.length bytes
    location <- randomRIO (1, byteLength)
    chV <- randomRIO (0, 255)
    return (replaceByte location chV bytes)


randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- randomReplaceByte imageFile
    let glitchedFileName = mconcat ["glitched_", fileName]  
    BC.writeFile glitchedFileName glitched 
    putStrLn "Готово!"

