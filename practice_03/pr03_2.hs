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


data StatE = Found | Finding deriving Show


getCurRoom :: Labir -> Room
getCurRoom (Labir room list) = room


getNextRooms :: Labir -> [Room]
getNextRooms (Labir room (r:rest)) = (getCurRoom r) : getNextRooms (Labir room rest)
getNextRooms (Labir room []) = []


goNextFirstRoom :: Maybe Labir -> Maybe Labir
goNextFirstRoom (Just (Labir room list)) = case length list of
                                        0 -> Nothing
                                        _ -> Just (head list)


toNextFirstRoom :: Labir -> Labir
toNextFirstRoom (Labir room list) = fromMaybe (Labir tupik []) (goNextFirstRoom (Just (Labir room list)))


{- findRoute :: RWS.RWS (Labir room list) [Room] StatE [Room]
findRoute = -}





rwsExample :: RWS.RWS Int [String] Int Int
rwsExample =
    RWS.ask >>= \coefficient ->                                 -- коэффициент из Reader
    RWS.get >>= \counter ->                                     -- текущее состояние (счетчик)
    RWS.put (counter + 1) >>                                    -- изменение состояния
    RWS.tell ["Increas counter to " ++ show (counter + 1)] >>   -- логирование действия
    return (coefficient * counter)                              -- результат (коэффициент * счетчик)

(resultRWS, logsRWS, finalStateRWS) = RWS.runRWS rwsExample 5 1



