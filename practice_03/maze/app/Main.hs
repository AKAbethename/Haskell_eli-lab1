module Main where


import Lib

import qualified Control.Monad.RWS as RWS

getThirdNormalEl :: (Bool, Path, Log) -> Log
getThirdNormalEl (x, y, z) = reverse z


main :: IO()
--main = putStrLn "Searching ..." >> print ( RWS.runRWS (explore labirint) labirint [] ) >> putStrLn "End" 
main = putStrLn "Maze is" >> print labirint >> putStrLn "Searching ..." >> putStrLn "Логирование перемещений: " 
                            >> print (getThirdNormalEl (RWS.runRWS (explore labirint) labirint [])) >> putStrLn "End" 
 



