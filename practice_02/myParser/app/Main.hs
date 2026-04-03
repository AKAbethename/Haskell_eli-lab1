module Main (main) where

import Lib
import MyTypes.MyEither
import MyTypes.MyMaybe
import MyTypes.MyTree

import qualified MyParsers.ParserEli as My
import qualified MyParsers.ParsecEli as P


runParser = My.runParser
plusOrMult = My.plusOrMult

runParserParsec = P.runParser
plusOrMultParsec = P.plusOrMultParsec



--main :: IO ()
--main = someFunc


main :: IO ()
main = do
    putStrLn "MyParser:"
    putStrLn $ show (runParser plusOrMult "12*345dsf")
    putStrLn $ show (runParser plusOrMult "12+345dsf")
    putStrLn "Parsec:"
    putStrLn $ show (P.runParser plusOrMultParsec "12*345dsf")
    putStrLn $ show (P.runParser plusOrMultParsec "12+345dsf")


