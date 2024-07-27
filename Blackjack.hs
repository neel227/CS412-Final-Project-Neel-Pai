module Blackjack where

import Control.Monad.State
import System.IO
import qualified Collection as C
import Probability
import System.Random


-- Expanding on the mini blackjack from Collections into full blackjack.
type Deck = C.Collection C.Card
type Hand = [C.Card]

data GameState = GameState {
    deck :: Deck,
    playerHand :: Hand,
    dealerHand :: Hand,
    rng :: StdGen
}

-- Initialize a standard deck of 52 cards
initDeck :: Deck
initDeck = C.deck

-- Shuffle the deck
shuffleDeck :: StateT GameState IO ()
shuffleDeck = do
    gs <- get
    let (shuffledDeck, newRng) = shuffle' (deck gs) (rng gs)
    put gs { deck = shuffledDeck, rng = newRng }


-- Shuffle function using the Fisher-Yates algorithm
shuffle' :: Deck -> StdGen -> (Deck, StdGen)
shuffle' deck = runState (shuffleM deck)

-- Fisher-Yates algorithm for shuffling
shuffleM :: [a] -> State StdGen [a]
shuffleM xs = foldM swap xs [0..n-1]
  where
    n = length xs
    swap ys i = do
        j <- state (randomR (i, n-1))
        return (swapelems i j ys)

-- Helper function to swap two elements in a list
swapelems :: Int -> Int -> [a] -> [a]
swapelems i j xs =
    let elemI = xs !! i
        elemJ = xs !! j
    in zipWith (\ k x -> (if k == i then elemJ else if k == j then elemI else x)) [0..] xs

-- Draw a card from the deck
drawCard :: StateT GameState IO C.Card
drawCard = do
    shuffleDeck
    gs <- get
    shuffleDeck
    let dist = C.selectOne (deck gs)
    (card, newDeck) <- liftIO $ evalStateT (randomSelect dist) (rng gs)
    put gs { deck = newDeck, rng = mkStdGen (round (fromIntegral (length newDeck) * 1.3)) } 
    return card

-- Function to extract a value from a distribution using the StateT monad
randomSelect :: Dist (a, b) -> StateT StdGen IO (a, b)
randomSelect dist = do
    rng <- get
    let results = unD dist
    let ((result, newDeck), newRng) = chooseRandom results rng
    put newRng
    return result

-- Helper function to choose a random element from a list of probabilities
chooseRandom :: [(a, Float)] -> StdGen -> ((a, Float), StdGen)
chooseRandom [] rng = error "Empty distribution"
chooseRandom xs rng =
    let (r, newRng) = randomR (0, 1) rng
        cumulativeProbs = scanl1 (\(_, acc) (x, p) -> (x, acc + p)) xs
        selected = head $ dropWhile (\(_, acc) -> acc < r) cumulativeProbs
    in (selected, newRng)

addToHand :: C.Card -> Hand -> Hand
addToHand card hand = card : hand


-- Calculate the score of a hand
calculateScore :: Hand -> Int
calculateScore hand = let
    rawScore = sum $ map C.value hand
    numAces = length $ filter (\(rank, _) -> rank == C.Ace) hand
    in if rawScore <= 21 then rawScore else adjustForAces rawScore numAces

adjustForAces :: Int -> Int -> Int
adjustForAces score numAces
    | score <= 21 = score
    | numAces == 0 = score
    | otherwise = adjustForAces (score - 10) (numAces - 1)

-- Display the hand
displayHand :: Hand -> String
displayHand hand = unwords $ map (\(rank, suit) -> show rank ++ " of " ++ show suit) hand

-- Check if busted
isBusted :: Hand -> Bool
isBusted hand = calculateScore hand > 21

-- Check if blackjack
isBlackjack :: Hand -> Bool
isBlackjack hand = calculateScore hand == 21
