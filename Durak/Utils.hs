module Durak.Utils
    ( cardsInHand
    , getPlayerById
    , getFirstUncoveredCard
    , getSuit
    , allCardsCovered
    , allCardsUncovered
    , nextCurrentPlayer
    , nextDefendingPlayer
    , nextActivePlayer
    , putAttackingCardOnTable
    , putDefendingCardOnTable
    , computerMovesDelay
    , shuffleList
    ) where

import Durak.Models
import Data.List
import Data.Maybe
import System.Random

cardsInHand :: Int
cardsInHand = 6

computerMovesDelay :: Int
computerMovesDelay = 5000000

shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList [x] = return [x]
shuffleList as = do
    i <- randomRIO (0,length as-1)
    shuffledRest <- shuffleList (take i as ++ drop (i+1) as)
    return $ (as !! i) : shuffledRest

getPlayerById :: Int -> [Player] -> Maybe Player
getPlayerById pid players = find (\ (Player playerId _ _ _) -> playerId == pid) players

getSuit :: Card -> Suit
getSuit (Card _ suit) = suit

getFirstUncoveredCard :: Table -> Maybe Card
getFirstUncoveredCard table =
    firstCardFromPair $ find (\ (CardPair firstCard secondCard) -> isNothing secondCard) table
    where
        firstCardFromPair Nothing = Nothing
        firstCardFromPair (Just (CardPair first _)) = Just first

allCardsCovered :: GameState -> Bool
allCardsCovered (GameState _ _ _ _ _ _ table) =
    all (\ (CardPair prevCard card) -> isJust card) table

allCardsUncovered :: GameState -> Bool
allCardsUncovered (GameState _ _ _ _ _ _ table) =
    all (\ (CardPair prevCard card) -> isNothing card) table


nextCurrentPlayer :: GameState -> GameState
nextCurrentPlayer (GameState currentPlayer@(Player plId _ _ _) defendingPlayer pls ro deck tr table) =
    (GameState newCurrentPlayer defendingPlayer newOtherPlayers ro deck tr table)
    where
        newOtherPlayers = if newCurrentPlayer == defendingPlayer
                            then pls ++ [currentPlayer]
                            else if currentPlayer == defendingPlayer
                                    then (pls \\ [newCurrentPlayer])
                                    else (pls \\ [newCurrentPlayer]) ++ [currentPlayer]
        newCurrentPlayer = nextActivePlayer plId (defendingPlayer:pls)

nextDefendingPlayer :: GameState -> GameState
nextDefendingPlayer (GameState currentPlayer defendingPlayer@(Player defPlId _ _ _) pls ro deck tr table) =
    (GameState currentPlayer newDefendingPlayer (pls \\ [newDefendingPlayer]) ro deck tr table)
    where
        newDefendingPlayer = nextActivePlayer defPlId (currentPlayer:pls)

nextActivePlayer :: Int -> [Player] -> Player
nextActivePlayer pid players = if isJust foundPlayer
                                then fromJust foundPlayer
                                else nextActivePlayer (pid + 1) players
    where
        foundPlayer = find (\ (Player plid _ _ hand) -> plid == (mod (pid + 1) 4)) players

putDefendingCardOnTable :: Card -> GameState -> GameState
putDefendingCardOnTable card (GameState cur (Player plId name isAi hand) pls ro deck tr table) =
    (GameState newDefendingPlayer newDefendingPlayer pls ro deck tr newTable)
    where
        newDefendingPlayer = (Player plId name isAi newHand)
        newTable =
            (take firstUncoveredPairIndex table) ++ [CardPair prevCard (Just card)] ++ (drop (firstUncoveredPairIndex + 1) table)
        firstUncoveredPair@(CardPair prevCard Nothing) = table !! firstUncoveredPairIndex
        firstUncoveredPairIndex = fromJust $ findIndex (\ (CardPair firstCard secondCard) -> isNothing secondCard) table
        newHand = filter (/=card) hand

putAttackingCardOnTable :: Card -> GameState -> GameState
putAttackingCardOnTable card (GameState (Player plId name isAi hand) def pls ro deck tr table) =
    (GameState (Player plId name isAi newHand) def pls ro deck tr newTable)
    where
        newTable = table ++ [CardPair card Nothing]
        newHand = filter (/=card) hand
