module Durak.GameLogic
    (startGame, startState, putCardOnTable) where

import Durak.Models
import Durak.UI
import Durak.Utils

startState :: GameState
startState =
    (GameState currentPlayer defendingPlayer otherPlayers 1 readyDeck trump [])
    where
        readyDeck = tail deckAfterGiveaway ++ [head deckAfterGiveaway]
        trump = getSuit $ head deckAfterGiveaway
        deckAfterGiveaway = drop 24 fullDeck
        currentPlayer = Player 1 "You" False (take 6 fullDeck)
        defendingPlayer = Player 2 "Trus" True (take 6 (drop 6 fullDeck))
        otherPlayers = [
            Player 3 "Balbes" True (take 6 (drop 12 fullDeck)),
            Player 4 "Byvaliy" True (take 6 (drop 18 fullDeck))]
        fullDeck = [Card rank suit | rank <- [Six .. Ace], suit <- [Clubs .. Spades]]

putCardOnTable :: Card -> GameState -> GameState
putCardOnTable card (GameState (Player plId name isAi hand) def pls ro deck tr table) =
    (GameState (Player plId name isAi newHand) def pls ro deck tr newTable)
    where
        newTable = case table of
            [] -> [CardPair card Nothing]
            curTable@((CardPair _ (Just _)):_) -> (CardPair card Nothing):curTable
            curTable@((CardPair prevCard Nothing):_) -> (CardPair prevCard (Just card)):(tail curTable)
        newHand = filter (/=card) hand

startGame :: IO ()
startGame = do
   mainCycle startState

mainCycle :: GameState -> IO ()
mainCycle state = do
    printState state
    newState <- nextMove state
    if gameOver newState
        then printLoser newState
        else mainCycle newState

nextMove :: GameState -> IO GameState
nextMove gameState@(GameState currentPlayer defendingPlayer _ _ _ _ _) = do
    if currentPlayer == defendingPlayer
        then nextDefendingMove gameState
        else nextAttackingMove gameState

nextDefendingMove :: GameState -> IO GameState
nextDefendingMove gameState@(GameState currentPlayer defendingPlayer _ _ _ _ _) = do
    return gameState


nextAttackingMove :: GameState -> IO GameState
nextAttackingMove gameState@(GameState (Player _ _ _ hand) _ _ _ _ _ _) = do
    cardNumber <- askForCardNumber
    let newGameState = putCardOnTable (hand !! cardNumber) gameState
    cardNumber <- askForOneMoreCardNumber
    if cardNumber == -1
        then return newGameState
        else return newGameState -- rethink this piece

gameOver :: GameState -> Bool
gameOver _ = False
