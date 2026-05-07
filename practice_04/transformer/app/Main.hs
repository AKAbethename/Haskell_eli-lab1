module Main where

import System.IO
import Control.Monad.Writer
import Control.Monad.Reader
-- import Control.Monad.Trans.State -- transformers
import Control.Monad.State -- mtl
import Control.Monad.Identity ( Identity(..) )

import Lib
import qualified MonadMyStateT as My




main :: IO ()
main = do
    labirint_raw <- readFile "src/maze_eli.txt"
    let labirint = read labirint_raw :: Labir
    print labirint
    putStrLn " "
    putStrLn " "

    result <- runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint) []) [start, chlb, msk, spb, kzn, kgd, ekb, uud, krd, smr, finish, mkhch, prm, ufa, grz, rst]
    print result
    putStrLn "Итог игры:"
    print $ fst $ fst result
    putStrLn "Состояния игры:"
    print $ snd $ fst $ result
    putStrLn "Лог выбора действий пользователя"
    print $ snd result
    


-- новая функция
-- result <- runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint) []) [start, chlb, msk, spb, kzn, kgd, ekb, uud, krd, smr, finish, mkhch, prm, ufa, grz, rst]

