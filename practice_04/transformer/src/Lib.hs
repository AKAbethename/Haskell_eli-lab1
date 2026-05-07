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
type Path   = [Room]

getCurRoom :: Labir -> Room
getCurRoom (Labir room list) = room

getNextRooms :: Labir -> [Room]
getNextRooms (Labir room (r:rest)) = (getCurRoom r) : getNextRooms (Labir room rest)
getNextRooms (Labir room []) = []


children :: Labir -> [Labir]
children (Labir room ch) = ch


explore :: Labir -> RWS.RWS Labir Log Path Bool
explore node =
  let name = getCurRoom node
  in
    if name == finish
    then RWS.modify (name :) >> RWS.tell [name] >> return True
    else exploreChildren (children node) >>= \found ->
           if found
           then RWS.modify (name :) >> RWS.tell [name] >> return True
           else return False

exploreChildren :: [Labir] -> RWS.RWS Labir Log Path Bool
exploreChildren [] = return False
exploreChildren (c:cs) =
  explore c >>= \found ->
  if found then return True
  else exploreChildren cs



{- choice :: Labir -> Either Int Room -> WriterT Log (ReaderT Labir Identity) Bool
choice room_s (Left k) = do
                            tell "Route is ended"
                            return (True)
choice room_s (Right room_to) = do -}



choice :: [Room] -> IO Room
choice cur_list = do
                    putStrLn "Input the next room"
                    city <- getLine
                    return city




 ----------------------------------------------------- НОВЫЕ ФУНКЦИИ -----------------------------------


{- getNextRooms :: Labir -> [Room]
getNextRooms (Labir room (r:rest)) = (getCurRoom r) : getNextRooms (Labir room rest)
getNextRooms (Labir room []) = [] -}


getNextRooms' :: Either String Labir -> [Room]
getNextRooms' (Left str) = []
getNextRooms' (Right (Labir room [])) = []
getNextRooms' (Right (Labir room (r:rest))) = ((getCurRoom r) : getNextRooms (Labir room rest))

{- toNextRoom :: Labir -> Room -> Either String Labir
toNextRoom (Labir room_s []) room_to = Left "Error" 

toNextRoom (Labir room_s (l:ls)) room_to = if getCurRoom l == room_to
                                            then Right l 
                                            else toNextRoom (Labir room_s ls) room_to -}

toNextRoom :: Labir -> Room -> Labir
toNextRoom (Labir room_s []) room_to = Labir tupik []
toNextRoom (Labir room_s (l:ls)) room_to = if getCurRoom l == room_to then l else toNextRoom (Labir room_s ls) room_to
-- здесь точно следующая комната есть, ошибки нету


toNextRoom' :: Either String Labir -> Room -> Either String Labir
toNextRoom' (Left "Error") _ = Left "Error"
toNextRoom' (Right (Labir room_s (l:ls))) room_to = if getCurRoom l == room_to
                                                 then Right l 
                                      --          else (toNextRoom' (Right (Labir room_s ls)) room_to)
                                                 else if length ls == 0 
                                                      then (toNextRoom' (Left "Error") room_to)
                                                      else (toNextRoom' (Right (Labir room_s ls)) room_to) 



choice_with_monad :: Room -> WriterT Log (ReaderT [Room] Identity) Bool
choice_with_monad our_choice = do
                        cur_list <- lift ask
                        let sign = elem our_choice cur_list 
                        if our_choice == gameOver then tell ["game over"] >> return True
                            else if (sign == False) then tell ["Error_eli"] >> return False else tell [our_choice] >> return True
                       -- return False


ask_neighbour_room :: IO Room
ask_neighbour_room = do
                    putStrLn "Введите:"
                    next_room_raw <- getLine
                    let next_room = next_room_raw :: Room
                    return next_room

{- 
go_to_next_room :: Labir -> Room -> My.MyStateT Path (WriterT Log (ReaderT [Room] IO) ) Bool
go_to_next_room cur_maze next_room = do
                    cur_next_rooms <- My.lift $ lift ask 
                    if getCurRoom cur_maze == finish || length cur_next_rooms == 0
                        then do
                                My.lift $ tell ["FINISH"]
                                My.modify (++ [finish])
                                return True
                        else if elem next_room cur_next_rooms 
                                then if getNextRooms cur_maze /= [] 
                                        then do 
                                                My.lift $ tell [next_room] 
                                                My.modify (++ [next_room])
                                                let next_maze = toNextRoom cur_maze next_room
                                                let next_rooms2 = getNextRooms next_maze
                                                liftIO $ putStrLn "Соседние комнаты"
                                                liftIO $ print next_rooms2
                                                liftIO $ putStrLn "Введите q, чтобы выйти"
                                                new_room <- liftIO ask_neighbour_room 
                                                go_to_next_room next_maze new_room
                                        else do 
                                                My.lift $ tell ["next is " ++ next_room]  
                                                My.modify (next_room :)
                                                return True
                                else if next_room /= ("q" :: Room) 
                                    then do
                                        My.lift $ tell ["error"] 
                                        My.modify (("empty room" :: Room) :)
                                        return False
                                    else do
                                        My.lift $ tell ["q"]
                                        My.modify (++ [gameOver])
                                        return False -}



-- runIdentity $ runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint chlb) []) [chlb, msk, spb]

-- result <- runReaderT (runWriterT $ My.runMyStateT (go_to_next_room labirint msk) []) [chlb, msk, spb, kzn, kgd, ekb, uud, krd, smr, finish, mkhch, prm, ufa, grz, rst]
                    
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
                                            



                        
                    



{- go_to_children :: [Labir] ->  -}

{- exploreChildren :: [Labir] -> RWS.RWS Labir Log Path Bool
exploreChildren [] = return False
exploreChildren (c:cs) =
  explore c >>= \found ->
  if found then return True
  else exploreChildren cs -}
