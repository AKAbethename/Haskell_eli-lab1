-------------------------------------
-- Практическое задание 3. Часть 2 --
-------------------------------------

module Pr03_2 where

import qualified Control.Monad.RWS as RWS
import Data.Maybe ( fromMaybe )

{-

Реализуйте поиск маршрута прохождения лабиринта с использованием монады RWS
    - карта лабиринта является внутренней переменной
    - каждая комната имеет уникальное текстовое название
    - маршрут начинается в комнате "старт"
    - маршрут заканчивается в комнате "финиш"
    - реализуйте поиск маршрута от "старта" до "финиша"
    - все перемещения логируются и выводятся на экран после завершения поиска
    - в реализации не использовать do нотацию

-}



type Room = String

data Labir = Labir Room [Labir] deriving Show

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


tupik = "tupik" :: Room


labirint = Labir start [Labir chlb [Labir ekb [Labir krd [Labir kzn [Labir smr [Labir ufa [], Labir finish [] ] ], Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ] ],
                        Labir msk [Labir kgd [Labir krd [Labir kzn [Labir smr [Labir ufa [], Labir finish [] ] ], Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ] ],
                        Labir spb [Labir kgd [Labir krd [Labir kzn [Labir smr [Labir ufa [], Labir finish [] ] ], Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ], 
                                    Labir uud [Labir rst [Labir grz [Labir smr [Labir ufa [], Labir finish []], Labir mkhch [Labir prm [Labir ufa [] ] ] ], Labir mkhch [Labir prm [Labir ufa [] ] ] ] ] ] ]


data StatE = Found | Finding | Tupik deriving Show


{- getCurRoom :: Labir -> Room
getCurRoom (Labir room list) = room


getNextRooms :: Labir -> [Room]
getNextRooms (Labir room (r:rest)) = (getCurRoom r) : getNextRooms (Labir room rest)
getNextRooms (Labir room []) = []


goNextFirstRoom :: Maybe Labir -> Maybe Labir
goNextFirstRoom (Just (Labir room list)) = case length list of
                                        0 -> Nothing
                                        _ -> Just (head list)


toNextFirstRoom :: Labir -> Labir
toNextFirstRoom (Labir room list) = fromMaybe (Labir tupik []) (goNextFirstRoom (Just (Labir room list))) -}


type Log    = [String]
type Path   = [Room]

getCurRoom :: Labir -> Room
getCurRoom (Labir room list) = room


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
           then RWS.modify (name :) >> RWS.tell ["<- " ++ name] >> return True
           else return False



exploreChildren :: [Labir] -> RWS.RWS Labir Log Path Bool
exploreChildren [] = return False
exploreChildren (c:cs) =
  explore c >>= \found ->
  if found then return True
  else exploreChildren cs
