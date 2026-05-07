module Lib where

import qualified Control.Monad.RWS as RWS
import System.IO
import Control.Monad.Writer
import Control.Monad.Reader
-- import Control.Monad.Trans.State -- transformers
-- import Control.Monad.State -- mtl
import Control.Monad.Trans.State
import Control.Monad.Identity ( Identity(..) )

import qualified MonadMyStateT as My

type Room = String

data Labir = Labir Room [Labir] deriving (Show, Read)

chlb = "CHLB" :: Room  
ekb = "EKB" :: Room
msk = "MSK" :: Room
kgd = "KGD" :: Room
spb = "SPB" :: Room
uud = "UUD" :: Room 
rst = "RST" :: Room
kzn = "KZN" :: Room
smr = "SMR" :: Room
ufa = "UFA" :: Room
prm = "PRM" :: Room
krd = "KRD" :: Room
grz = "GRZ" :: Room
mkhch = "MKHCH" :: Room

start = "START" :: Room
finish = "FINISH" :: Room

gameOver = "GAMEOVER" :: Room
tupik = "tupik" :: Room


labirint = Labir start [Labir chlb [Labir ekb [Labir krd [Labir kzn [Labir smr [Labir ufa [], Labir finish [] ] ], Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ] ],
                        Labir msk [Labir kgd [Labir krd [Labir kzn [Labir smr [Labir ufa [], Labir finish [] ] ], Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ] ],
                        Labir spb [Labir kgd [Labir krd [Labir kzn [Labir smr [Labir ufa [], Labir finish [] ] ], Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ], 
                                    Labir uud [Labir rst [Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ] ]


data StatE = Found | Finding | Tupik | Lose deriving Show


type Log    = [String]

getCurRoom :: Labir -> Room
getCurRoom (Labir room list) = room

getNextRooms :: Labir -> [Room]
getNextRooms (Labir room (r:rest)) = (getCurRoom r) : getNextRooms (Labir room rest)
getNextRooms (Labir room []) = []



 ----------------------------------------------------- НОВЫЕ ФУНКЦИИ -----------------------------------


toNextRoom :: Labir -> Room -> Labir
toNextRoom (Labir room_s []) room_to = Labir tupik []
toNextRoom (Labir room_s (l:ls)) room_to = if getCurRoom l == room_to then l else toNextRoom (Labir room_s ls) room_to
-- здесь точно следующая комната есть, ошибки нету



ask_neighbour_room :: IO Room
ask_neighbour_room = do
                    putStrLn "Введите:"
                    next_room_raw <- getLine
                    let next_room = next_room_raw :: Room
                    return next_room



go_to_next_room :: Labir -> My.MyStateT [StatE] (WriterT Log (ReaderT [Room] IO) ) Bool
go_to_next_room cur_maze = do
                access_rooms <- My.lift $ lift ask
                if getCurRoom cur_maze == finish  -- если текущая комната -- финиш
                    then do
                          My.lift $ tell [] -- или gameOver
                          My.modify (++ [Found])
                          return True
                else  -- не финиш => q или комната
                    if elem (getCurRoom cur_maze) access_rooms -- если комната из данных комнат (внешнее окружение)
                        then do
                                let next_rooms = getNextRooms cur_maze
                                if length next_rooms /= 0   -- если есть следующие комнаты из этой
                                    then do
                                 --       liftIO $ print next_rooms
                                        liftIO $ putStrLn "Введите название следующей комнаты, 'look' для просмотра соседних комнат или 'q' для выхода"
                                        next_room <- liftIO ask_neighbour_room
                                        if next_room == ("look" :: Room) 
                                            then do
                                                liftIO $ print next_rooms
                                                My.lift $ tell ["look"]
                                                liftIO $ putStrLn "Введите название следующей комнаты или 'q' для выхода"
                                                next_room <- liftIO ask_neighbour_room
                                                My.lift $ tell [next_room]
                                                My.modify (++ [Finding])
                                                let new_maze = toNextRoom cur_maze next_room
                                                go_to_next_room new_maze
                                            else do
                                                My.lift $ tell [next_room]
                                                My.modify (++ [Finding])
                                                let new_maze = toNextRoom cur_maze next_room
                                                go_to_next_room new_maze
                                    else do
                                        My.lift $ tell []
                                        My.modify (++ [Lose])
                                        return False
                                {- My.lift & tell next_rooms
                                My.modify (++ [next_room])
                                return True -}
                        else -- не финиш и не в списке данных комнат
                            if getCurRoom cur_maze == "q"
                                then do
                                        My.lift $ tell ["q"]
                                        My.modify (++ [Found])
                                        return True
                                else do   --  не финиш и не в списке данных комнат и не q => некорректная комната
                                        My.lift $ tell []
                                        My.modify (++ [Lose])
                                        return False
                                            

-- новая функция
-- result <- runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint) []) [start, chlb, msk, spb, kzn, kgd, ekb, uud, krd, smr, finish, mkhch, prm, ufa, grz, rst]
