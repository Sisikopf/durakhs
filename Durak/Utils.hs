module Durak.Utils
    (getPlayerNameById) where

import Durak.Models

getPlayerNameById :: Int -> [Player] -> String
getPlayerNameById playerId players =
    getPlayerName $ head $ filter (\ player -> (getPlayerId player) == playerId) players
