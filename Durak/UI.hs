module Durak.UI
    ( printState
    , printLoser
    , askForStartAttackingMove
    , askForContinueAttackingMove
    , askForStartDefendingMove
    , askForContinueDefendingMove
    ) where

import System.Console.ANSI
import Durak.Models
import Durak.Utils

printState :: GameState -> IO()
printState (GameState currentPlayer defendingPlayer otherPlayers roundNum deck trump table) = do
    clearScreen
    printRoundNum roundNum
    printCurrentPlayer currentPlayer
    printDefendingPlayer defendingPlayer
    printDeck deck
    printTrump trump
    printTable table
    printPlayers (currentPlayer:defendingPlayer:otherPlayers)
    setCursorPosition 26 0

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
    setCursorPosition 12 20
    putStr $ show table

printPlayers :: [Player] -> IO()
printPlayers players = do
    setCursorPosition 22 50
    printPlayer $ getPlayerById 0 players
    setCursorPosition 12 0
    printPlayer $ getPlayerById 1 players
    setCursorPosition 0 50
    printPlayer $ getPlayerById 2 players
    setCursorPosition 12 100
    printPlayer $ getPlayerById 3 players

printPlayer :: Player -> IO()
printPlayer (Player _ name False hand) = putStr $ show hand
printPlayer (Player _ name True hand) = putStr $ name ++ " " ++ (show $ length hand)

printCurrentPlayer :: Player -> IO()
printCurrentPlayer (Player _ name _ _) = do
    setCursorPosition 1 0
    putStr $ "Current player: " ++ name

printDefendingPlayer ::  Player -> IO()
printDefendingPlayer (Player _ name _ _) = do
    setCursorPosition 2 0
    putStr $ "Defending player: " ++ name

printLoser :: GameState -> IO()
printLoser (GameState currentPlayer defendingPlayer otherPlayers _ _ _ _) = do
    setCursorPosition 50 50
    putStr $ (show $ name $ head (filter (\ (Player _ _ _ hand)-> length hand /= 0) (currentPlayer:defendingPlayer:otherPlayers))) ++ " lost"

askForStartAttackingMove :: GameState -> IO Card
askForStartAttackingMove (GameState (Player _ _ _ hand) _ _ _ _ _ _) = do
    putStr "Choose card from your hand (1-N): "
    cardNumStr <- getLine
    let cardNum = read cardNumStr :: Int
    return $ hand !! (cardNum - 1)

askForContinueAttackingMove :: GameState -> IO (Maybe Card)
askForContinueAttackingMove (GameState (Player _ _ _ hand) _ _ _ _ _ _) = do
    putStr "Choose one more card from your hand or finish your move (0): "
    cardNumStr <- getLine
    let cardNum = read cardNumStr :: Int
    return $ if cardNum == 0 then Nothing else Just (hand !! (cardNum - 1))

askForStartDefendingMove :: GameState -> IO Card
askForStartDefendingMove (GameState (Player _ _ _ hand) _ _ _ _ _ _) = do
    putStr "Choose card from your hand (1-N): "
    cardNumStr <- getLine
    let cardNum = read cardNumStr :: Int
    return $ hand !! (cardNum - 1)

askForContinueDefendingMove :: GameState -> IO (Maybe Card)
askForContinueDefendingMove (GameState (Player _ _ _ hand) _ _ _ _ _ _) = do
    putStr "Choose one more card from your hand or finish your move (0): "
    cardNumStr <- getLine
    let cardNum = read cardNumStr :: Int
    return $ if cardNum == 0 then Nothing else Just (hand !! (cardNum - 1))
