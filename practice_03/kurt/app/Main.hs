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

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- randomReplaceByte imageFile
    let glitchedFileName = "glitched_" ++ fileName  -- проще, чем mconcat
    BC.writeFile glitchedFileName glitched   -- исправлено: вместо main
    putStrLn "Готово!"

