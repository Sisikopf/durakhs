module Durak.Utils
    ( getPlayerById ) where

import Durak.Models

getPlayerById :: Int -> [Player] -> Player
getPlayerById pid players =
    head $ filter (\ (Player playerId _ _ _) -> playerId == pid) players
