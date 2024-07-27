module Dice where

import Probability
import Data.List (groupBy, sortOn)
import Data.Function (on)

type Die = Int

die :: Dist Die
die = uniform [1..6]

twoDice :: Dist (Die,Die)
twoDice = prod die die

dice :: Int -> Dist [Die]
dice 0 = certainly []
dice n = joinWith (:) die (dice (n-1))


twoSixes = (==[6,6]) ?? dice 2

-- sixes p n computes the probability of getting
-- p sixes (>1,==2,...) when rolling n dice

sixes :: (Int -> Bool) -> Int -> Probability
sixes p n = (p . length . filter (==6)) ?? dice n

plus1 x = choose 0.5 x (x+1)

droll = do
        d <- die
        plus1 d

g3 = (>3) ?? die

addTwo = do
        d1 <- die
        d2 <- die
        return (d1+d2)

-- Built by Neel!
-- Function to calculate the distribution of sums for a given number of dice
diceSums :: Int -> Dist Int
diceSums n = toDist . map convertBack . sumGrouped . map convert . unD . mapD sum $ dice n
  where
    convert :: (Int, ProbRep) -> (Int, Probability)
    convert (x, p) = (x, P p)

    convertBack :: (Int, Probability) -> (Int, ProbRep)
    convertBack (x, P p) = (x, p)

    sumGrouped :: [(Int, Probability)] -> [(Int, Probability)]
    sumGrouped dist = map (\xs -> (fst (head xs), P (sum (map ((\(P p) -> p) . snd) xs))))
                       . groupBy ((==) `on` fst)
                       . sortOn fst
                       $ dist

    toDist :: [(Int, ProbRep)] -> Dist Int
    toDist = D
