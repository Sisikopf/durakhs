module Durak.Utils
    ( getPlayerById
    , getFirstUncoveredCard
    , nextDefendingPlayer
    , nextActivePlayer ) where

import Durak.Models
import Data.List
import Data.Maybe

getPlayerById :: Int -> [Player] -> Maybe Player
getPlayerById pid players =
    find (\ (Player playerId _ _ _) -> playerId == pid) players

getFirstUncoveredCard :: Table -> Maybe Card
getFirstUncoveredCard table =
    firstCardFromPair $ find (\ (CardPair firstCard secondCard) -> isNothing secondCard) table
    where
        firstCardFromPair Nothing = Nothing
        firstCardFromPair (Just (CardPair first _)) = Just first


nextDefendingPlayer :: GameState -> GameState
nextDefendingPlayer (GameState currentPlayer defendingPlayer@(Player defPlId _ _ _) pls ro deck tr table) =
    (GameState currentPlayer newDefendingPlayer (pls \\ [defendingPlayer]) ro deck tr table)
    where
        newDefendingPlayer = nextActivePlayer defPlId (currentPlayer:pls)

nextActivePlayer :: Int -> [Player] -> Player
nextActivePlayer pid players = if isJust foundPlayer
                                then fromJust foundPlayer
                                else nextActivePlayer (pid + 1) players
    where
        foundPlayer = find
                        (\ (Player plid _ _ hand) -> plid == (mod (pid + 1) 4))
                        players
