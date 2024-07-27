module MontyHall where


import Probability hiding (choose)
import Data.List ( (\\) )
import Control.Monad (liftM)

type Door = Int 

doors :: Int -> [Door]
doors n = [1..n]

data State = Doors {prize :: Door, chosen :: Door, opened :: Door}
             deriving (Eq,Ord,Show)


-- initial configuration of the game status
--  
start :: State
start = Doors {prize=u,chosen=u,opened=u} where u=undefined


-- Steps of the game:
-- 
-- (1) hide the prize
-- (2) choose a door
-- (3) open a non-open door, not revealing the prize
-- (4) apply strategy: switch or stay
-- 
hide :: Int -> Trans State
hide n s = Probability.uniform [s { prize = d } | d <- doors n]

choose :: Int -> Trans State
choose n s = Probability.uniform [s {chosen = d} | d <- doors n]

open :: Int -> Trans State
open n s = Probability.uniform [s {opened = d} | d <- doors n \\ [prize s,chosen s]]

type Strategy = Int -> Trans State

switch :: Int -> Trans State
switch n s = Probability.uniform [s { chosen = d } | d <- doors n \\ [chosen s, opened s]]


stay :: Strategy
stay _ = idT

-- data State = Doors {prize :: Door, chosen :: Door, opened :: Door}
--data Dist a = D {unD :: [(a,ProbRep)]}
--            | C (Float -> Float)

-- Int -> Int -> State -> Dist State
game :: Int -> Strategy -> Trans State
game n s = sequ [hide n,choose n,open n, s n]


-- Playing the game
-- 
data Outcome = Win | Lose
               deriving (Eq,Ord,Show)

result :: State -> Outcome
result s = if chosen s==prize s then Win else Lose

eval :: Int -> Strategy -> Dist Outcome
eval n s = mapD result (game n s start)

simEval :: Int -> Int -> Strategy -> RDist Outcome
simEval k n s = mapD result `fmap` (k ~. game n s) start


-- Alternative modeling
--
firstChoice :: Int -> Dist Outcome
firstChoice n = uniform (Win: replicate (n-1) Lose)

switch' :: Trans Outcome
switch' Win  = certainly Lose
switch' Lose = certainly Win
