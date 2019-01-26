module Durak.Validations
    ( isCorrectCardIndex
    , canPutAttackingCardOnTable
    , thereIsCardWithSameRankOnTable
    , isCorrectDefendingCard
    , isTransitAllowed) where

import Durak.Models
import Durak.Utils
import Data.Maybe

isCorrectCardIndex :: Maybe Int -> Hand -> Bool
isCorrectCardIndex Nothing _ = False
isCorrectCardIndex (Just cardIndex) hand = cardIndex >= 1 && cardIndex <= (length hand)

canPutAttackingCardOnTable :: Hand -> Table -> Bool
canPutAttackingCardOnTable defHand table =
    pairsLength < 6 && uncoveredPairsLength < defHandLength
    where
        uncoveredPairsLength = length $ filter (\ (CardPair firstCard secondCard) -> isNothing secondCard) table
        pairsLength = length table
        defHandLength = length defHand

thereIsCardWithSameRankOnTable :: Card -> Table -> Bool
thereIsCardWithSameRankOnTable (Card rank _ ) table =
    any (\ (CardPair (Card firstRank _) maybeCard) -> firstRank == rank || (sameRankWithMaybeCard rank maybeCard)) table
    where
        sameRankWithMaybeCard _ Nothing = False
        sameRankWithMaybeCard rank (Just (Card anotherRank _)) = rank == anotherRank

isCorrectDefendingCard :: Card -> Card -> Trump -> Bool
isCorrectDefendingCard (Card coveringCardRank coveringCardSuit) (Card cardToCoverRank cardToCoverSuit) trump
    | cardToCoverSuit == coveringCardSuit && coveringCardRank > cardToCoverRank = True
    | coveringCardSuit == trump && cardToCoverSuit /= trump = True
    | otherwise = False

isTransitAllowed :: GameState -> Bool
isTransitAllowed gameState =
    ro /= 1 && ((length table) + 1) <= (length defHand)
    where
        (GameState _ (Player _ _ _ defHand) _ ro _ _ table) = nextDefendingPlayer gameState
