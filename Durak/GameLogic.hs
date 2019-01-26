module Durak.GameLogic
    ( startGame ) where

import Durak.Models
import Durak.UI
import Durak.Validations
import Durak.Utils
import Data.Maybe
import Data.List
import Control.Concurrent

startState :: Deck -> GameState
startState shuffledDeck =
    (GameState currentPlayer defendingPlayer otherPlayers 1 readyDeck trump [])
    where
        readyDeck = tail deckAfterGiveaway ++ [head deckAfterGiveaway]
        trump = getSuit $ head deckAfterGiveaway
        deckAfterGiveaway = drop (4*cardsInHand) shuffledDeck
        currentPlayer = Player 0 "You" False (take cardsInHand shuffledDeck)
        defendingPlayer = Player 1 "Trus" True (take cardsInHand (drop cardsInHand shuffledDeck))
        otherPlayers = [
            Player 2 "Balbes" True (take cardsInHand (drop (2*cardsInHand) shuffledDeck)),
            Player 3 "Byvaliy" True (take cardsInHand (drop (3*cardsInHand) shuffledDeck))]

startGame :: IO ()
startGame = do
   shuffledDeck <- shuffleList [Card rank suit | rank <- [Eight .. Ace], suit <- [Clubs .. Spades]]
   mainCycle $ startState shuffledDeck

mainCycle :: GameState -> IO ()
mainCycle state = do
    printState state
    if gameOver state
        then printGameOver state
        else do
            newState <- nextMove state
            mainCycle newState

nextMove :: GameState -> IO GameState
nextMove gameState@(GameState currentPlayer defendingPlayer _ prevRound _ _ _) = do
    newGameState@(GameState _ _ _ newRound _ _ _) <- if currentPlayer == defendingPlayer
                                                        then nextDefendingMove gameState
                                                        else nextAttackingMove gameState
    return $ (if prevRound == newRound
                then nextCurrentPlayer newGameState
                else nextRound newGameState)

nextAttackingMove :: GameState -> IO GameState
nextAttackingMove gameState@(GameState _ _ _ _ _ _ table) = do
    newnewGameState <- (if table == []
                            then do
                                newGameState <- startAttackingMove gameState
                                continueAttackingMove newGameState
                            else continueAttackingMove gameState)
    printState newnewGameState
    return newnewGameState

startAttackingMove :: GameState -> IO GameState
startAttackingMove gameState@(GameState (Player _ _ isAi _) _ _ _ _ _ _) =
    if isAi
        then startComputerAttackingMove gameState
        else startHumanAttackingMove gameState

continueAttackingMove :: GameState -> IO GameState
continueAttackingMove gameState@(GameState (Player _ _ isAi _) _ _ _ _ _ _) =
    if isAi
        then continueComputerAttackingMove gameState
        else continueHumanAttackingMove gameState

nextDefendingMove :: GameState -> IO GameState
nextDefendingMove gameState = do
    newGameState <- startDefendingMove gameState
    newNewGameState <- continueDefendingMove newGameState
    printState newNewGameState
    return newNewGameState

startDefendingMove :: GameState -> IO GameState
startDefendingMove gameState@(GameState (Player _ _ isAi _) _ _ _ _ _ _) = do
    if isAi
        then startComputerDefendingMove gameState
        else startHumanDefendingMove gameState

continueDefendingMove :: GameState -> IO GameState
continueDefendingMove gameState@(GameState (Player _ _ isAi _) _ _ _ _ _ _) = do
    if isAi
        then continueComputerDefendingMove gameState
        else continueHumanDefendingMove gameState

allCardsCovered :: GameState -> Bool
allCardsCovered (GameState _ _ _ _ _ _ table) =
    all (\ (CardPair prevCard card) -> isJust card) table

allCardsUncovered :: GameState -> Bool
allCardsUncovered (GameState _ _ _ _ _ _ table) =
    all (\ (CardPair prevCard card) -> isNothing card) table

nextRound :: GameState -> GameState
nextRound gameState = if curHand == []
                        then nextCurrentPlayer $ nextDefendingPlayer gameStateWithoutInactivePlayersAndEmptyTable
                        else gameStateWithoutInactivePlayersAndEmptyTable
    where
        gameStateWithoutInactivePlayersAndEmptyTable@(GameState (Player _ _ _ curHand) _ _ _ _ _ _) =
            nextDefendingPlayer (GameState cur def (filter (\ (Player _ _ _ hand) -> hand /= []) pls) ro deck trump [])
        (GameState cur def pls ro deck trump table) = takeCardsFromDeck gameState

takeCardsFromTable :: GameState -> GameState
takeCardsFromTable (GameState (Player plId name isAi hand) def pls ro deck tr table) =
    (GameState curPlayer curPlayer pls (ro + 1) deck tr [])
    where
        curPlayer = (Player plId name isAi (hand ++ cardsOnTable))
        cardsOnTable = concat $ map (\ cardPair@(CardPair card maybeCard) ->
                            if isJust maybeCard
                                then [card, (fromJust maybeCard)]
                                else [card]) table

takeCardsFromDeck :: GameState -> GameState
takeCardsFromDeck gameState@(GameState cur def pls _ _ _ _) =
    foldl takeCardsFromDeckForPlayer gameState (pls ++ [def])

takeCardsFromDeckForPlayer :: GameState -> Player -> GameState
takeCardsFromDeckForPlayer gameState@(GameState cur def pls ro deck tr table) player@(Player pid name isAi hand)
    |    cur == player || def == player = GameState newPlayer newPlayer pls ro newDeck tr table
    |    otherwise = let playerIndex = fromJust (elemIndex player pls) in
                GameState cur def ((take playerIndex pls) ++ [newPlayer] ++ (drop (playerIndex + 1) pls)) ro newDeck tr table
    where
        newPlayer = Player pid name isAi newHand
        newDeck = drop cardsToTake deck
        newHand = hand ++ (take cardsToTake deck)
        cardsToTake = max 0 (cardsInHand - (length hand))


transitCard :: Card -> GameState -> GameState
transitCard card gameState =
    nextCurrentPlayer $ nextDefendingPlayer $ putAttackingCardOnTable card gameState

gameOver :: GameState -> Bool
gameOver (GameState cur def pls _ deck _ _) =
    deck == [] && length (filter (\ (Player _ _ _ hand) -> hand /= []) (cur:def:pls)) < 2

startHumanAttackingMove :: GameState -> IO GameState
startHumanAttackingMove gameState = do
    card <- askForStartAttackingMove gameState
    return $ putAttackingCardOnTable card gameState

continueHumanAttackingMove :: GameState -> IO GameState
continueHumanAttackingMove gameState = do
    printState gameState
    card <- askForContinueAttackingMove gameState
    if isJust card
        then continueAttackingMove (putAttackingCardOnTable (fromJust card) gameState)
        else return gameState

startHumanDefendingMove :: GameState -> IO GameState
startHumanDefendingMove gameState@(GameState cur def pls ro deck tr table) = do
    if allCardsCovered gameState
        then return $ GameState cur def pls (ro + 1) deck tr table
        else do
            action <- if allCardsUncovered gameState
                then askForCoverTakeOrTransitCards gameState
                else askForCoverOrTakeCards gameState
            return (case action of
                Take -> nextCurrentPlayer $ nextDefendingPlayer $ takeCardsFromTable gameState
                Transit card -> transitCard card gameState
                Cover card -> putDefendingCardOnTable card gameState)

continueHumanDefendingMove :: GameState -> IO GameState
continueHumanDefendingMove gameState = do
    printState gameState
    if allCardsCovered gameState
        then return gameState
        else do
            action <- askForCoverOrTakeCards gameState
            case action of
                Take -> return $ nextCurrentPlayer $ nextDefendingPlayer $ takeCardsFromTable gameState
                Cover card -> continueDefendingMove (putDefendingCardOnTable card gameState)

startComputerAttackingMove :: GameState -> IO GameState
startComputerAttackingMove gameState@(GameState (Player _ name _ hand) _ _ _ _ trump _) = do
    putStr $ name ++ " thinks..."
    threadDelay(computerMovesDelay)
    let card = findCardWithMinimumRank hand trump
    return $ putAttackingCardOnTable card gameState

continueComputerAttackingMove :: GameState -> IO GameState
continueComputerAttackingMove gameState@(GameState (Player _ name _ curHand) (Player _ _ _ defHand) _ _ _ trump table@((CardPair card _):_)) = do
    printState gameState
    putStr $ name ++ " thinks..."
    threadDelay(computerMovesDelay)
    if canPutAttackingCardOnTable defHand table
        then do
            let card = findCardToContinueAttack curHand table trump
            if isJust card
                then continueComputerAttackingMove (putAttackingCardOnTable (fromJust card) gameState)
                else return gameState
        else return gameState

startComputerDefendingMove :: GameState -> IO GameState
startComputerDefendingMove gameState@(GameState cur def@(Player _ name _ hand) pls ro deck tr table@((CardPair card _):_)) = do
    if allCardsCovered gameState
        then return $ GameState cur def pls (ro + 1) deck tr table
        else do
            putStr $ name ++ " thinks..."
            threadDelay(computerMovesDelay)
            let foundCard = findCardToTransit hand card tr
            if isJust foundCard
                then return $ transitCard (fromJust foundCard) gameState
                else do
                    let foundCard = findCardToCover hand (fromJust $ getFirstUncoveredCard table) tr
                    if isJust foundCard
                        then return $ putDefendingCardOnTable (fromJust foundCard) gameState
                        else return $ nextCurrentPlayer $ nextDefendingPlayer $ takeCardsFromTable gameState

continueComputerDefendingMove :: GameState -> IO GameState
continueComputerDefendingMove gameState@(GameState (Player _ name _ hand) _ _ _ _ tr table@((CardPair card _):_)) = do
    printState gameState
    if allCardsCovered gameState
        then return gameState
        else do
            putStr $ name ++ " thinks..."
            threadDelay(computerMovesDelay)
            let foundCard = findCardToCover hand (fromJust $ getFirstUncoveredCard table) tr
            if isJust foundCard
                then continueDefendingMove $ putDefendingCardOnTable (fromJust foundCard) gameState
                else return $ nextCurrentPlayer $ nextDefendingPlayer $ takeCardsFromTable gameState

findCardWithMinimumRank :: Hand -> Trump -> Card
findCardWithMinimumRank hand trump =
    minimum $ if notTrumps == [] then hand else notTrumps
    where
        notTrumps = filter (\ (Card _ suit) -> suit /= trump) hand

findCardToContinueAttack :: Hand -> Table -> Trump -> Maybe Card
findCardToContinueAttack [] _ _ = Nothing
findCardToContinueAttack hand table trump =
    find (\ card -> thereIsCardWithSameRankOnTable card table)
         (sort $ filter (\ (Card _ suit) -> suit /= trump) hand)

findCardToTransit :: Hand -> Card -> Trump -> Maybe Card
findCardToTransit [] _ _ = Nothing
findCardToTransit hand (Card rank _) trump =
    find (\ (Card r _) -> r == rank)
         (filter (\ (Card _ suit) -> suit /= trump) hand)

findCardToCover :: Hand -> Card -> Trump -> Maybe Card
findCardToCover [] _ _ = Nothing
findCardToCover hand cardToCover trump =
    if isJust suitableNotTrump
        then suitableNotTrump
        else suitableTrump
    where
        suitableTrump = find (\ coveringCard -> isCorrectDefendingCard coveringCard cardToCover trump) trumps
        suitableNotTrump = find (\ coveringCard -> isCorrectDefendingCard coveringCard cardToCover trump) notTrumps
        trumps = sort $ filter (\ (Card _ suit) -> suit == trump) hand
        notTrumps = sort $ filter (\ (Card _ suit) -> suit /= trump) hand
