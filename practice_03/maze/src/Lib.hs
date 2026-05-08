module Lib where

import qualified Control.Monad.RWS as RWS

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
           then RWS.modify (name :) >> RWS.tell [name] >> return True
           else return False

exploreChildren :: [Labir] -> RWS.RWS Labir Log Path Bool
exploreChildren [] = return False
exploreChildren (c:cs) =
  explore c >>= \found ->
  if found then return True
  else exploreChildren cs


