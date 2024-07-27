module Lib where

import System.Random

getRandomNumber :: IO Int
getRandomNumber = do
    gen <- newStdGen
    let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
    return randNumber