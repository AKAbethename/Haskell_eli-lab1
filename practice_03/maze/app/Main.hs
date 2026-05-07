module Main where


import Lib

import qualified Control.Monad.RWS as RWS


main :: IO()
--main = putStrLn "Searching ..." >> print ( RWS.runRWS (explore labirint) labirint [] ) >> putStrLn "End" 
main = putStrLn "Maze is" >> print (labirint) >> putStrLn "Searching ..." >> print (RWS.runRWS (explore labirint) labirint []) >> putStrLn "End" 
 



