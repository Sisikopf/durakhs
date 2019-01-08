module Durak.GameLogic
    (startGame, startState, putCardOnTable) where

import Durak.Models
import Durak.UI

startState :: GameState
startState =
    (GameState 1 2 1 initializedPlayers readyDeck trump [])
    where
        readyDeck = tail deckAfterGiveaway ++ [head deckAfterGiveaway]
        trump = getSuit $ head deckAfterGiveaway
        deckAfterGiveaway = drop 24 fullDeck
        initializedPlayers = [
            Human 1 "You" (take 6 fullDeck),
            AI 2 "Trus" (take 6 (drop 6 fullDeck)),
            AI 3 "Balbes" (take 6 (drop 12 fullDeck)),
            AI 4 "Byvaliy" (take 6 (drop 18 fullDeck))]
        fullDeck = [Card rank suit | rank <- [Six .. Ace], suit <- [Clubs .. Spades]]

putCardOnTable :: Card -> GameState -> GameState
putCardOnTable card (GameState cur def ro pls deck tr table) =
    (GameState cur def ro pls deck tr newTable)
    where newTable = case table of
            [] -> [CardPair card Nothing]
            curTable@((CardPair _ (Just _)):_) -> (CardPair card Nothing):curTable
            curTable@((CardPair prevCard Nothing):_) -> (CardPair prevCard (Just card)):(tail curTable)

startGame :: IO()
startGame = do
   printState startState
