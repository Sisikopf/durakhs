module Durak.Utils
    ( getPlayerById ) where

import Durak.Models
import Data.List

getPlayerById :: Int -> [Player] -> Maybe Player
getPlayerById pid players =
    find (\ (Player playerId _ _ _) -> playerId == pid) players
