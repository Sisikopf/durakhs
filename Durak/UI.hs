module Durak.UI
    ( printState
    , printGameOver
    , askForStartAttackingMove
    , askForContinueAttackingMove
    , askForCoverTakeOrTransitCards
    , askForCoverOrTakeCards
    ) where

import System.Console.ANSI
import Durak.Models
import Durak.Utils
import Durak.Validations
import Data.Maybe
import Data.List
import Text.Read

printState :: GameState -> IO()
printState gameState@(GameState currentPlayer defendingPlayer otherPlayers roundNum deck trump table) = do
    printStateDebugInfo gameState
    clearScreen
    printRoundNum roundNum
    printCurrentPlayer currentPlayer
    printDefendingPlayer defendingPlayer
    printDeck deck
    printTrump trump
    printTable table
    printPlayers (currentPlayer:defendingPlayer:otherPlayers)
    setCursorPosition 20 0

printStateDebugInfo :: GameState -> IO()
printStateDebugInfo (GameState currentPlayer defendingPlayer otherPlayers roundNum deck trump table) = do
    putStrLn $ "========================="
    putStrLn $ "Current player: " ++ (getPlayerDebugInfo currentPlayer)
    putStrLn $ "Defending player: " ++ (getPlayerDebugInfo defendingPlayer)
    putStrLn $ "Other players: " ++ (concat $ map getPlayerDebugInfo otherPlayers)
    putStrLn $ "Deck: " ++ (concat $ map show deck)
    putStrLn $ "========================="

getPlayerDebugInfo :: Player -> String
getPlayerDebugInfo (Player pid name isAi hand) = (show pid) ++ " " ++ name ++ " " ++ (show isAi) ++ " " ++ (show hand) ++ " "

printRoundNum :: Int -> IO()
printRoundNum roundNum = do
    setCursorPosition 0 0
    putStr $ "Round " ++ (show roundNum)

printDeck :: Deck -> IO()
printDeck deck = do
    setCursorPosition 0 100
    putStr $ "Deck " ++ (show $ length deck)

printTrump :: Suit -> IO()
printTrump trump = do
    setCursorPosition 1 100
    putStr $ "Trump " ++ (show trump)

printTable :: Table -> IO()
printTable table = do
    setCursorPosition 9 30
    putStr $ show table

printPlayers :: [Player] -> IO()
printPlayers players = do
    setCursorPosition 18 40
    printPlayer $ getPlayerById 0 players
    setCursorPosition 9 0
    printPlayer $ getPlayerById 1 players
    setCursorPosition 0 50
    printPlayer $ getPlayerById 2 players
    setCursorPosition 9 90
    printPlayer $ getPlayerById 3 players

printPlayer :: Maybe Player -> IO()
printPlayer Nothing = putStr ""
printPlayer (Just (Player _ _ False hand)) = putStr $ show hand
printPlayer (Just (Player _ name True hand)) = putStr $ name ++ " " ++ (show $ length hand)

printCurrentPlayer :: Player -> IO()
printCurrentPlayer (Player _ name _ _) = do
    setCursorPosition 1 0
    putStr $ "Current player: " ++ name

printDefendingPlayer ::  Player -> IO()
printDefendingPlayer (Player _ name _ _) = do
    setCursorPosition 2 0
    putStr $ "Defending player: " ++ name

printGameOver :: GameState -> IO()
printGameOver (GameState currentPlayer defendingPlayer otherPlayers _ _ _ _) = do
    setCursorPosition 15 50
    putStrLn $ "Game Over. " ++ (if isJust loser then show (name (fromJust loser)) ++ " lost." else "Draw.")
    where
        loser = find (\ (Player _ _ _ hand) -> hand /= []) (currentPlayer:defendingPlayer:otherPlayers)

askForStartAttackingMove :: GameState -> IO Card
askForStartAttackingMove gameState@(GameState (Player _ _ _ hand) _ _ _ _ _ _) = do
    putStr "Choose card from your hand (1-N): "
    cardIndexStr <- getLine
    let cardIndex = readMaybe cardIndexStr :: Maybe Int
    if isCorrectCardIndex cardIndex hand
        then return $ hand !! ((fromJust cardIndex) - 1)
        else do
            putStrLn "Incorrect input!"
            askForStartAttackingMove gameState

askForContinueAttackingMove :: GameState -> IO (Maybe Card)
askForContinueAttackingMove gameState@(GameState (Player _ _ _ hand) (Player _ _ _ defHand) _ _ _ _ table) = do
    putStr "Choose one more card from your hand (1-N) or finish your move (f): "
    cardIndexStr <- getLine
    case cardIndexStr of
        "f" -> return Nothing
        otherwise ->
            let cardIndex = readMaybe cardIndexStr :: Maybe Int in
            if isCorrectCardIndex cardIndex hand
                then if canPutAttackingCardOnTable defHand table
                        then
                            let chosenCard = hand !! ((fromJust cardIndex) - 1) in
                            if thereIsCardWithSameRankOnTable chosenCard table
                            then return $ Just chosenCard
                            else do
                                putStrLn $ "There is no card on table with the same rank as " ++ show chosenCard
                                askForContinueAttackingMove gameState
                        else do
                            putStrLn "You can't put one more card on table! Finish your turn."
                            askForContinueAttackingMove gameState
                else do
                    putStrLn "Incorrect input!"
                    askForContinueAttackingMove gameState

askForCoverTakeOrTransitCards :: GameState -> IO DefendingAction
askForCoverTakeOrTransitCards gameState@(GameState (Player _ _ _ hand) _ _ _ _ trump table@((CardPair (Card firstCardRank _) _):_)) = do
    putStr "Choose one card from your hand (1-N) or take cards from table (ta): "
    action <- getLine
    case action of
        "ta" -> return Take
        otherwise -> do
            let cardIndex = readMaybe action :: Maybe Int
            if isCorrectCardIndex cardIndex hand
                then do
                    let card@(Card rank _) = hand !! ((fromJust cardIndex) - 1)
                    if rank == firstCardRank
                            then do
                                action <- isTransitOrDefend
                                case action of
                                    "t" -> if isTransitAllowed gameState
                                               then return $ Transit card
                                               else do
                                                   putStrLn $ "You can't transit cards." ++
                                                    " It's the first round or next player has too few cards in hand."
                                                   askForCoverTakeOrTransitCards gameState
                                    "d" -> tryToCover card
                            else tryToCover card
                else do
                    putStrLn "Incorrect input!"
                    askForCoverTakeOrTransitCards gameState
    where
        tryToCover card = let firstUncoveredCard = fromJust $ getFirstUncoveredCard table in
                          if isCorrectDefendingCard card firstUncoveredCard trump
                              then return $ Cover card
                              else do
                                  putStrLn $ "You can't cover " ++ (show firstUncoveredCard) ++ " with " ++ (show card)
                                  askForCoverTakeOrTransitCards gameState

isTransitOrDefend :: IO String
isTransitOrDefend = do
    putStr "Do you want to transit the card or defend (t/d): "
    result <- getLine
    case result of
        "t" -> return result
        "d" -> return result
        otherwise -> isTransitOrDefend

askForCoverOrTakeCards :: GameState -> IO DefendingAction
askForCoverOrTakeCards gameState@(GameState (Player _ _ _ hand) _ _ _ _ trump table) = do
    putStr "Choose one card from your hand (1-N) or take cards from table (ta): "
    action <- getLine
    case action of
        "ta" -> return Take
        otherwise -> do
            let cardIndex = readMaybe action :: Maybe Int
            if isCorrectCardIndex cardIndex hand
                then do
                     let card = hand !! ((fromJust cardIndex) - 1)
                     let firstUncoveredCard = fromJust $ getFirstUncoveredCard table
                     if isCorrectDefendingCard card firstUncoveredCard trump
                         then return $ Cover card
                         else do
                             putStrLn $ "You can't cover " ++ (show firstUncoveredCard) ++ " with " ++ (show card)
                             askForCoverOrTakeCards gameState
                else do
                    putStrLn "Incorrect input!"
                    askForCoverOrTakeCards gameState
