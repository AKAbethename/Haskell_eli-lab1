import System.Environment
import System.Random (randomRIO)
import qualified Data.ByteString.Char8 as BC

import Lib

randomReplaceByte :: BC.ByteString -> IO (Int, BC.ByteString)
randomReplaceByte bytes = do
    let byteLength = BC.length bytes
    location <- randomRIO (1, byteLength)
    chV <- randomRIO (0, 255)
    return (chV, (replaceByte location chV bytes))


randomSortSection :: BC.ByteString -> IO (Int, BC.ByteString)
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (start, (sortSection start sectionSize bytes))

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    (change1, glitched1) <- randomReplaceByte imageFile
    (change2, glitched2) <- randomSortSection glitched1
    let strInt = intToChar change2
    let glitchedFileName = mconcat ["glitched_", [strInt], "_", fileName ]  
    BC.writeFile glitchedFileName glitched2 
    putStrLn "Готово!"

