{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Coupon where

import System.Random
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Probability
import Control.Monad.State

-- Function to simulate collecting coupons with a given distribution
-- Function to simulate collecting coupons with a given distribution
collectCoupons :: Dist Int -> StdGen -> Int
collectCoupons dist gen =
    let coupons = randomRsDist dist gen
    in countCoupons (supportSize dist) Set.empty coupons 0

-- Helper function to generate an infinite list of random numbers based on the distribution
randomRsDist :: Dist a -> StdGen -> [a]
randomRsDist dist = evalState (randomSelectLoop dist)

randomSelectLoop :: Dist a -> State StdGen [a]
randomSelectLoop dist = do
    result <- randomSelect dist
    results <- randomSelectLoop dist
    return (result : results)
    
-- Function to select a random item from the distribution
randomSelect :: Dist a -> State StdGen a
randomSelect (D xs) = do
    rng <- get
    let (r, newRng) = randomR (0.0, 1.0) rng
    put newRng
    return (selectByProbability r xs)

-- Helper function to select an item based on a random value and cumulative probabilities
selectByProbability :: Float -> [(a, Float)] -> a
selectByProbability r xs = fst . head . dropWhile ((< r) . snd) $ scanl1 (\(_, acc) (x, p) -> (x, acc + p)) xs

-- Function to count the number of coupons collected
countCoupons :: Int -> Set Int -> [Int] -> Int -> Int
countCoupons n collected (c:cs) count
    | Set.size collected == n = count
    | otherwise = countCoupons n (Set.insert c collected) cs (count + 1)

-- Function to run simulations and return results
runSimulations :: Dist Int -> Int -> Int -> [Int]
runSimulations dist sim seed = map (collectCoupons dist) gens
  where
    -- Generate a list of distinct random generators for each simulation
    gens = take sim $ iterate (snd . next) (mkStdGen seed)

-- Utility function to get the number of distinct elements in the distribution
supportSize :: Dist a -> Int
supportSize (D xs) = length xs

-- Calculate the number of elements, mean, and standard deviation based on output of simulation runs.
calculateStats :: [Int] -> IO (Integer, Double, Double)
calculateStats xs = do
    let n = fromIntegral $ length xs
    let mean = fromIntegral (sum xs) / n
    let variance = sum (map (\x -> (fromIntegral x - mean) ^ 2) xs) / n
    let stddev = sqrt variance
    return (round n, mean, stddev)

-- Convert simulation output into IO form
listToIO :: [Int] -> IO [Int]
listToIO = return

-- Function to calculate the Harmonic number
harmonicNumber :: Int -> Double
harmonicNumber n = sum [1 / fromIntegral k | k <- [1..n]]

--Coupon expectation function
expectedCoupons :: Int -> Double
expectedCoupons n = fromIntegral n * harmonicNumber n