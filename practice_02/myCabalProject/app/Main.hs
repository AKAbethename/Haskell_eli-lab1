module Main where

import MyEvolModule
import Data.List (sort)

main :: IO ()
--main = putStrLn "Hello, Haskell!"

main = putStrLn $ show $ sort ([LUCA, Cyanobacteria, Trilobite, Ichthyostega, Dimetrodon, Archaeopteryx, Morganucodon, Purgatorius,
                     Australopithecine, Humans] :: [MyEvolution])
