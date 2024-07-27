module Barber where

import Probability
import Queuing
import ListUtils

-- barber shop

custServ :: Dist Time
custServ = normal [10..40]

nextCust :: Trans Time
nextCust _ = normal [5..20]

--barbers :: Int
--barbers = 2
--
--customers :: Int
--customers = 30
--
--runs :: Int
--runs = 5

barberEvent :: ((), (Dist Time, Trans Time))
barberEvent =  unit (custServ, nextCust)

--barberEvents :: [((), (Dist Time, Trans Time))]
--barberEvents = replicate customers barberEvent
barberEvents :: Int -> [((), (Dist Time, Trans Time))]
barberEvents customers = replicate customers barberEvent

--barberSystem :: (System () -> b) -> RDist b
--barberSystem = evalSystem runs barbers barberEvents

barberSystem :: Int -> Int -> Int -> (System () -> b) -> RDist b
barberSystem runs barbers customers = evalSystem runs barbers (barberEvents customers)


-- category

data Category = ThreeOrLess | FourToTen | MoreThanTen deriving (Eq,Ord,Show)

cat :: Time -> Category
cat n | n <= 3 = ThreeOrLess
cat n | n <= 10 = FourToTen
cat _ = MoreThanTen

perc :: Float -> String
perc n | n <= 0.25 = "0% to 25%"
perc n | n <= 0.5 = "25% to 50%"
perc n | n <= 0.75 = "50% to 75%"
perc _ = "75% to 100%"

-- avg barber idle time
--barberIdle :: RDist String
--barberIdle = barberSystem (perc.idleAvgP barbers)
barberIdle :: Int -> Int -> Int -> RDist String
barberIdle runs barbers customers = barberSystem runs barbers customers (perc . idleAvgP barbers)


-- avg customer waiting time (unserved customers)
--customerWait :: RDist Category
--customerWait = barberSystem ( cat.(`div` customers).waiting barbers )
customerWait :: Int -> Int -> Int -> RDist Category
customerWait runs barbers customers = barberSystem runs barbers customers (cat . (`div` customers) . waiting barbers)


-- evaluation
eval :: Int -> Int -> Int -> (RDist String, RDist Category)
eval runs barbers customers = (barberIdle runs barbers customers, customerWait runs barbers customers)
--eval :: (RDist String, RDist Category)
--eval = (barberIdle, customerWait)
