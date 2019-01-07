module Durak.UI
    ( printState ) where

import System.Console.ANSI
import Durak.Models
import Durak.Utils

printState :: GameState -> IO()
printState (GameState currentPlayerId defendingPlayerId roundNum players deck trump table) = do
    clearScreen
    printRoundNum roundNum
    printCurrentPlayer currentPlayerId players
    printDefendingPlayer defendingPlayerId players
    printDeck deck
    printTrump trump
    printTable table
    printPlayers players
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
    setCursorPosition 25 50
    printPlayer $ players !! 0
    setCursorPosition 12 0
    printPlayer $ players !! 1
    setCursorPosition 0 50
    printPlayer $ players !! 2
    setCursorPosition 12 100
    printPlayer $ players !! 3

printPlayer :: Player -> IO()
printPlayer (Human _ name hand) = putStr $ show hand
printPlayer (AI _ name hand) = putStr $ name ++ " " ++ (show $ length hand)

printCurrentPlayer :: Int -> [Player] -> IO()
printCurrentPlayer playerId players = do
    setCursorPosition 1 0
    putStr $ "Current player: " ++ (getPlayerNameById playerId players)

printDefendingPlayer :: Int -> [Player] -> IO()
printDefendingPlayer playerId players = do
    setCursorPosition 2 0
    putStr $ "Defending player: " ++ (getPlayerNameById playerId players)
