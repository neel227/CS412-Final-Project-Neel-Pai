module Main where

--Basic Libraries
import System.Random
import System.IO
import Data.Functor
import Data.Typeable 
import Control.Monad.Trans.State
import Control.Monad.Trans
import Text.Printf (printf)

-- Game

-- Files for probabalistic functions and types
import Lib
import Probability
import Visualize

-- Provided PFP Files for specific probability questions
import MontyHall
import Barber
import NBoys
import Dice
import TreeGrowth

-- Custom files
import Blackjack
import Coupon

n = 10

printLn :: String -> IO ()
printLn str = putStrLn str >> hFlush stdout

-- Function to print a variable type
printType :: Typeable a => a -> IO ()
printType x = putStrLn $ "Type: " ++ show (typeOf x)

-- Function to prompt for input and read an integer
getInt :: String -> IO Int
getInt prompt = do
    putStr prompt
    hFlush stdout  
    input <- getLine
    return (read input :: Int)

class ValidInput a where
    validate :: a -> a -> Bool
    errorMessage :: a -> String

instance ValidInput Int where
    validate minVal value = value >= minVal
    errorMessage minVal = "Please enter a number greater than or equal to " ++ show minVal ++ ":"

instance ValidInput Float where
    validate minVal value = value >= minVal
    errorMessage minVal = "Please enter a number greater than or equal to " ++ show minVal ++ ":"

-- Helper function to get a valid input within a range
getValidStart :: (Read a, ValidInput a) => a -> IO a
getValidStart minVal = do
    input <- getLine
    case reads input of
        [(value, "")] -> if validate minVal value
            then return value
            else do
                putStrLn $ errorMessage minVal
                getValidStart minVal
        _ -> do
            putStrLn "Invalid input. Please enter a valid value:"
            getValidStart minVal


-- Helper function to get a valid input within a range
getValidInput :: Int -> Int -> IO Int
getValidInput minVal maxVal = do
    input <- getLine
    let value = read input :: Int
    if value >= minVal && value <= maxVal
        then return value
        else do
            putStrLn $ "Invalid input. Please enter a number between " ++ show minVal ++ " and " ++ show maxVal ++ ":"
            getValidInput minVal maxVal

-- Helper function to choose a random element from a list
randomChoice :: [a] -> IO a
randomChoice xs = do
    index <- randomRIO (0, length xs - 1)
    return (xs !! index)

-- Helper Function for Yes/No handling
yesorno :: IO Bool
yesorno = do
    input <- getLine
    if input == "y" || input == "Yes" || input == "yes" || input == "Y"
        then return True
    else if input == "n" || input == "No" || input == "no" || input == "N"
        then return False
    else do
        putStrLn "Invalid input. Please enter 'y' for yes or 'n' for no:"
        yesorno


-- Helper function to get a valid input within a range
getDistributionInput ::  IO String
getDistributionInput  = do
    distribution <- getLine
    if distribution == "Uniform" || distribution == "uniform" || distribution == "u" || distribution == "U"
        || distribution == "Beta" || distribution == "beta" || distribution == "b" || distribution == "B"
        || distribution == "Normal" || distribution == "normal" || distribution == "n" || distribution == "N"
        then return distribution
    else do
        putStrLn $ "Invalid input. Please enter uniform, beta, or normal."
        getDistributionInput


--Helper Function for Formatting
formatDouble :: Double -> Int -> String
formatDouble number decimalPlaces = printf ("%." ++ show decimalPlaces ++ "f") number

-- Test to perform the beta test
betaTest :: IO ()
betaTest = do
    putStrLn "Beta testing"
    putStrLn "What is the alpha input?"
    a <- getValidStart 1
    putStrLn "What is the beta input?"
    b <- getValidStart 1
    let betaDist = beta a b
    printSpread betaDist [0, 0.01 .. 0.1]
    result2 <- pick (betaDist [0, 0.01 .. 0.1])
    print result2

-- Function to perform the barber test
barberTest :: IO ()
barberTest = do    

    putStrLn "How many barbers are there? (must have at least 1)"
    barbers <- getValidStart 1

    putStrLn "How many customers are there throughout the day? (must have at least 1)"
    customers <- getValidStart 1

    putStrLn "How many runs to simulate? (must be at least 1)"
    runs <- getValidStart 1

    putStrLn "Barber Shop Simulation Results:"
    putStrLn "Average Barber Idle Time Distribution:"
    
    -- Barber idle
    idleDist <- barberIdle runs barbers customers
    putStrLn $ distToString idleDist

    -- Extract the distribution of customer waiting times
    waitDist <- customerWait runs barbers customers
    putStrLn "Average Customer Wait Time Distribution:"
    putStrLn $ distToString waitDist

-- Function to perform the NBoys test
nBoysTest :: IO ()
nBoysTest = do
    putStrLn "Select the number of boys we know are in the family."
    k <- getValidStart 0

    putStrLn "Select the number of boys we are trying to figure out are in the family."
    m <- getValidStart k

    putStrLn "Select the total number of kids in the family."
    n <- getValidStart m

    let nboyprob = nBoys n k m
    putStrLn "NBoys Probability results"
    putStrLn $ "The probability that a family of " ++ show n ++ " children has at least " ++ show m 
    putStrLn $ " boys, given that there are at least " ++ show k ++ " boys is: " ++ show nboyprob

-- Function to perform the sumDice test
diceTest :: IO ()
diceTest = do
    putStrLn "Select the number of dice we want to sum together."
    numDice <- getValidStart 1
    let dist = diceSums numDice
    putStrLn "Distribution of possible sum values and their associated probabilities:"
    putStrLn $ distToString dist

-- Function for evaluating Monty Hall strategy probabilities
montyHallTest :: IO ()
montyHallTest = do
    putStrLn "Select the number of doors to test. This number must be an integer that is at least 3"
    numberOfDoors <- getValidStart 3
    -- Evaluate the 'stay' strategy
    let stayResults = MontyHall.eval numberOfDoors stay
    -- Evaluate the 'switch' strategy
    let switchResults = MontyHall.eval numberOfDoors switch
    -- Print the results
    putStrLn "Monty Hall Problem Results:"
    putStrLn "With Stay Strategy:"
    print stayResults
    putStrLn "\nWith Switch Strategy:"
    print switchResults    

-- Function to Play the Monty Hall Game
playMontyHall :: IO ()
playMontyHall = do
    -- Step 0: Choose a number of doors
    putStrLn "Select the number of doors in the game. This number must be an integer that is at least 3"
    numberOfDoors <- getValidStart 3

    -- Step 1: User selects an initial door
    putStrLn "Select an initial door (1 to number of doors):"
    initialDoor <- getValidInput 1 numberOfDoors

    -- Step 2: Machine selects a door to open (cannot be user's door or prize door)
    let possibleDoors = [1..numberOfDoors]
    prizeDoor <- randomRIO (1, numberOfDoors)
    let remainingDoors = filter (\d -> d /= initialDoor && d /= prizeDoor) possibleDoors
    openedDoor <- randomChoice remainingDoors

    -- Step 3: Inform the user about the opened door and ask if they want to switch
    putStrLn $ "Door " ++ show openedDoor ++ " is opened and it does not have the prize."
    putStrLn "Do you want to switch doors? (yes/no)"
    switchChoice <- yesorno

    -- Step 4: If user wants to switch, they must choose a new door
    finalDoor <- if switchChoice
        then do
            putStrLn "Select a new door (cannot be the same as the initial or opened door):"
            newDoor <- getValidInput 1 numberOfDoors
            return newDoor
        else return initialDoor

    -- Step 5: Determine if the user wins or loses
    putStrLn $ "Selected Door: " ++ show finalDoor
    putStrLn $ "Prize Door: " ++ show prizeDoor
    
    if finalDoor == prizeDoor
        then putStrLn "You win!"
        else putStrLn "You lose! Better luck next time."


-- Test for Tree Growth
treeGrowthTest :: IO ()
treeGrowthTest = do
    putStrLn "Select the number of generations you want to model for the tree."
    generations <- getValidStart 0

    putStrLn "Select the number of simulations you want to model."
    simulations <- getValidStart 0

    let history = simHist simulations generations seed  -- Simulate the tree's evolution
    -- Further code to visualize or process the history
    putStrLn $ "Simulated history over " ++ show generations ++ " generations with " ++ show simulations ++ " simulations."
    -- Assuming printR or another function to visualize the result
    printR history


couponTest :: IO ()
couponTest = do

    -- Allow choice of how we distribute coupon likelihoods
    putStrLn "Choose how we want to distribute the likelihood of getting a coupon: uniform, normal, or beta?"
    putStrLn "Note: Beta and normal can take a long time to load, so I would recommend staying with uniform."
    input <- getDistributionInput
    (distType, params) <- 
        if head input == 'n' || head input == 'N' then do
            putStrLn "Enter the mean (must be at least 0)"
            mean <- getValidStart 0
            putStrLn "Enter the standard deviation (must be at least 0.1)"
            sd <- getValidStart 0.1
            return ("normal", (mean, sd))
        else if head input == 'b' || head input == 'B' then do
            putStrLn "Enter the alpha parameter (must be at least 0.1)"
            alpha <- getValidStart 0.1
            putStrLn "Enter the beta parameter (must be at least 0.1)"
            beta <- getValidStart 0.1
            return ("beta", (alpha, beta))
        else return ("uniform", (0,0))

    putStrLn "Enter the number of distinct coupon types (must be at least 1):"
    n <- getValidStart 1
    putStrLn "Enter the number of simulations to run (must be at least 1):"
    sim <- getValidStart 1

    -- Create the distribution
    
    let dist = case distType of
            "normal" -> let (mean, sd) = params in Probability.shape (Probability.normalCurve mean sd) [1..n]
            "beta" -> let (alpha, beta) = params in Probability.shape (Probability.betaDensity alpha beta) [1..n]
            "uniform" -> Probability.uniform [1..n]

    -- Run the simulations
    gen <- newStdGen
    let (seed, _) = randomR (1, 1000) gen :: (Int, StdGen)
    let output =  runSimulations dist sim seed
    
    stats <- calculateStats output
    let (sim, mean, stddev) = stats

    putStrLn $ "Number of simulations performed: " ++ show sim
    putStrLn $ "Simulated mean number of coupons required to achieve the full set: " ++ show (formatDouble mean 2)
    putStrLn $ "Theoretical average number of coupons required to achieve the full set: " ++ show (formatDouble (expectedCoupons n) 2)
    putStrLn $ "Standard Deviation: " ++ show (formatDouble stddev 2)

    putStrLn "Would you like the individual number of draws needed per simulation?:"
    answer <- yesorno
    if answer then do
        putStrLn "Number of draws needed for each simulation:"
        results <- listToIO output
        mapM_ print results
    else     putStrLn ""



-- Blackjack Setup
-- Game loop
gameLoop :: StateT GameState IO ()
gameLoop = do
    gs <- get
    shuffleDeck
    liftIO $ putStrLn "Your hand:"
    liftIO $ putStrLn $ displayHand (playerHand gs)
    liftIO $ putStrLn $ "Your score: " ++ show (calculateScore (playerHand gs))
    if isBusted (playerHand gs) then liftIO $ putStrLn "You busted!" else if isBlackjack (playerHand gs) then liftIO $ putStrLn "Blackjack!" else playerTurn

playerTurn :: StateT GameState IO ()
playerTurn = do
    gs <- get
    shuffleDeck
    liftIO $ putStrLn "Do you want to hit or stand? (h/s)"
    choice <- liftIO getLine
    if choice == "h" then do
        card <- drawCard
        shuffleDeck
        put gs { playerHand = addToHand card (playerHand gs) }
        gameLoop
    else dealerTurn

dealerTurn :: StateT GameState IO ()
dealerTurn = do
    gs <- get
    liftIO $ putStrLn "Dealer's turn."
    dealerPlays

dealerPlays :: StateT GameState IO ()
dealerPlays = do
    gs <- get
    if calculateScore (dealerHand gs) < 17 then do
        card <- drawCard
        put gs { dealerHand = addToHand card (dealerHand gs) }
        dealerPlays
    else do
        let playerScore = calculateScore (playerHand gs)
        let dealerScore = calculateScore (dealerHand gs)
        liftIO $ putStrLn $ "Dealer's hand: " ++ displayHand (dealerHand gs)
        liftIO $ putStrLn $ "Dealer's score: " ++ show dealerScore
        liftIO $ if dealerScore > 21 || playerScore > dealerScore
            then putStrLn "You win!"
            else if playerScore == dealerScore
                then putStrLn "It's a tie!"
                else putStrLn "Dealer wins!"

--run BlackJack
runBlackJack :: StateT GameState IO ()
runBlackJack = do
    shuffleDeck
    gs <- get
    playerCard1 <- drawCard
    playerCard2 <- drawCard
    dealerCard1 <- drawCard
    dealerCard2 <- drawCard
    put gs { playerHand = [playerCard1, playerCard2], dealerHand = [dealerCard1, dealerCard2] }
    liftIO $ putStrLn "Welcome to Blackjack!"
    gameLoop

main :: IO ()
main = do

    gen <- newStdGen
    let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
    putStrLn $ "Random number from 1 to 10: " ++ show randNumber

    gen <- newStdGen
    let initialState = GameState {
            deck = initDeck,
            playerHand = [],
            dealerHand = [],
            rng = gen
        }
    
    putStrLn $ "Enter a number to determine which program to test!"
    putStrLn $ "0: Beta Test"
    putStrLn $ "1: Barber Test"
    putStrLn $ "2: NBoys Test"
    putStrLn $ "3: Dice Test"
    putStrLn $ "4: Monty Hall Test"
    putStrLn $ "5: Play Monty Hall"
    putStrLn $ "6: Tree Test"
    putStrLn $ "7: Coupon Test"
    putStrLn $ "8: Blackjack"
    putStrLn $ "If you do not want to play, enter 9"
    
    input <- getLine
    if input == "0" then betaTest
    else if input == "1" then barberTest
    else if input == "2" then nBoysTest
    else if input == "3" then diceTest
    else if input == "4" then montyHallTest
    else if input == "5" then playMontyHall
    else if input == "6" then treeGrowthTest
    else if input == "7" then couponTest
    else if input == "8" then evalStateT runBlackJack initialState
    else if input == "9" then putStrLn ""
    else putStrLn "Not recognized"

    putStrLn "Would you like to play anything else? (yes/no)"
    shouldRerun <- yesorno
    if shouldRerun then main else putStrLn "Thank you for playing!"
