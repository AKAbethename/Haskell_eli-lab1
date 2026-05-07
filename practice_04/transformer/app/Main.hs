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
    print(getCurRoom labirint)
    let cur_list = getNextRooms' (Right labirint)
    print cur_list
    print(labirint)
    print (" ")
    print (" ")
    next_room <- choice cur_list
  --  if(elem next_room cur_list) then print next_room else print "Error"
--    print $ runIdentity $ runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint next_room) []) [chlb, msk, spb]

    result <- runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint) []) [start, chlb, msk, spb, kzn, kgd, ekb, uud, krd, smr, finish, mkhch, prm, ufa, grz, rst]
    print $ snd result

-- go_to_next_room :: Labir -> Room -> My.MyStateT Path (WriterT Log (ReaderT [Room] Identity) ) Bool


-- новая функция
-- result <- runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint) []) [start, chlb, msk, spb, kzn, kgd, ekb, uud, krd, smr, finish, mkhch, prm, ufa, grz, rst]

