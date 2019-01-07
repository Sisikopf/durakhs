module Durak.GameLogic
    (startState) where

import Durak.Models

startState :: GameState
startState = GameState {
                currentPlayerId = 1,
                defendingPlayerId = 2,
                roundNum = 1,
                players = [
                    Human 1 "You" [Card Six Hearts, Card Ace Spades],
                    AI 2 "Trus" [Card Seven Spades],
                    AI 3 "Balbes" [Card Jack Diamonds],
                    AI 4 "Byvaliy" [Card Ace Clubs]
                ],
                deck = [Card Ten Hearts, Card Jack Clubs, Card Six Diamonds],
                trump = Clubs,
                table = [
                    CardPair (Card Eight Hearts) (Just (Card Jack Hearts)),
                    CardPair (Card Eight Diamonds) Nothing
                ]
             }
