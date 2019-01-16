module Durak.GameLogic
    (startGame) where

import Durak.Models
import Durak.UI
import Durak.Utils
import Data.Maybe
import Data.List

startState :: GameState
startState =
    (GameState currentPlayer defendingPlayer otherPlayers 1 readyDeck trump [])
    where
        readyDeck = tail deckAfterGiveaway ++ [head deckAfterGiveaway]
        trump = getSuit $ head deckAfterGiveaway
        deckAfterGiveaway = drop 24 fullDeck
        currentPlayer = Player 0 "You" False (take 6 fullDeck)
        defendingPlayer = Player 1 "Trus" True (take 6 (drop 6 fullDeck))
        otherPlayers = [
            Player 2 "Balbes" True (take 6 (drop 12 fullDeck)),
            Player 3 "Byvaliy" True (take 6 (drop 18 fullDeck))]
        fullDeck = [Card rank suit | rank <- [Two .. Ace], suit <- [Clubs .. Spades]]

putDefendingCardOnTable :: Card -> GameState -> GameState
putDefendingCardOnTable card (GameState (Player plId name isAi hand) def pls ro deck tr table) =
    (GameState (Player plId name isAi newHand) def pls ro deck tr newTable)
    where
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


startGame :: IO ()
startGame = do
   mainCycle startState

mainCycle :: GameState -> IO ()
mainCycle state = do
    printState state
    newState <- nextMove state
    if gameOver newState
        then printGameOver newState
        else mainCycle newState

nextMove :: GameState -> IO GameState
nextMove gameState@(GameState currentPlayer defendingPlayer _ prevRound _ _ _) = do
    newGameState@(GameState _ _ _ newRound _ _ _) <- if currentPlayer == defendingPlayer
                                                        then nextDefendingMove gameState
                                                        else nextAttackingMove gameState
    return $ (if prevRound == newRound
                then nextPlayer newGameState
                else nextRound newGameState)

nextPlayer :: GameState -> GameState
nextPlayer (GameState currentPlayer@(Player plId name isAi hand) defendingPlayer@(Player defPlId _ _ _) pls ro deck tr table) =
    (GameState newCurrentPlayer defendingPlayer newOtherPlayers ro deck tr table)
    where
        newOtherPlayers = if newPlId == defPlId
                            then pls ++ [currentPlayer]
                            else if plId == defPlId
                                    then tail pls
                                    else (tail pls) ++ [currentPlayer]
        newCurrentPlayer@(Player newPlId name isAi newHand) =
            head $ filter (\ (Player pid _ _ _) -> pid == mod (plId + 1) 4) (defendingPlayer:pls)

nextDefendingPlayer :: GameState -> GameState
nextDefendingPlayer (GameState currentPlayer@(Player plId name isAi hand) defendingPlayer@(Player defPlId _ _ _) pls ro deck tr table) =
    (GameState currentPlayer newDefendingPlayer (tail pls) ro deck tr table)
    where
        newDefendingPlayer@(Player newPlId name isAi newHand) =
            head $ filter (\ (Player pid _ _ _) -> pid == mod (plId + 1) 4) (currentPlayer:pls)


nextAttackingMove :: GameState -> IO GameState
nextAttackingMove gameState@(GameState _ _ _ _ _ _ table) = do
    newGameState <- case table of
        [] -> do
             newGameState <- startAttackingMove gameState
             continueAttackingMove newGameState
        _ -> continueAttackingMove gameState
    printState newGameState
    return newGameState

startAttackingMove :: GameState -> IO GameState
startAttackingMove gameState = do
    card <- askForStartAttackingMove gameState
    return $ putAttackingCardOnTable card gameState

continueAttackingMove :: GameState -> IO GameState
continueAttackingMove gameState = do
    printState gameState
    card <- askForContinueAttackingMove gameState
    if isJust card
        then continueAttackingMove (putAttackingCardOnTable (fromJust card) gameState)
        else return gameState

nextDefendingMove :: GameState -> IO GameState
nextDefendingMove gameState = do
    newGameState <- startDefendingMove gameState
    newNewGameState <- continueDefendingMove newGameState
    printState newNewGameState
    return newNewGameState

startDefendingMove :: GameState -> IO GameState
startDefendingMove gameState@(GameState cur def pls ro deck tr table) = do
    if allCardsCovered gameState
        then do
            printNextRound
            return $ GameState cur def pls (ro + 1) deck tr table
        else do
            action <- if allCardsUncovered gameState
                then askForCoverTakeOrTransitCards gameState
                else askForCoverOrTakeCards gameState
            return (case action of
                Take -> takeCardsFromTable gameState
                Transit card -> transitCard card gameState
                Cover card -> putDefendingCardOnTable card gameState)

continueDefendingMove :: GameState -> IO GameState
continueDefendingMove gameState = do
    printState gameState
    if allCardsCovered gameState
        then return gameState
        else do
            action <- askForCoverOrTakeCards gameState
            case action of
                Take -> return $ takeCardsFromTable gameState
                Cover card -> continueDefendingMove (putDefendingCardOnTable card gameState)

allCardsCovered :: GameState -> Bool
allCardsCovered (GameState _ _ _ _ _ _ table) =
    all (\ (CardPair prevCard card) -> isJust card) table

allCardsUncovered :: GameState -> Bool
allCardsUncovered (GameState _ _ _ _ _ _ table) =
    all (\ (CardPair prevCard card) -> isNothing card) table

nextRound :: GameState -> GameState
nextRound gameState = GameState cur def pls ro deck trump []
    where
    --    changePlayers
    --    removePlayers?
        (GameState cur def pls ro deck trump table) = nextDefendingPlayer stateAfterTookCards
        stateAfterTookCards = takeCardsFromDeck gameState

takeCardsFromTable :: GameState -> GameState
takeCardsFromTable (GameState (Player plId name isAi hand) def pls ro deck tr table) =
    (GameState (Player plId name isAi (hand ++ cardsOnTable)) def pls (ro + 1) deck tr [])
    where
        cardsOnTable = concat $ map (\ cardPair@(CardPair card maybeCard) ->
                            if isJust maybeCard
                                then [card, (fromJust maybeCard)]
                                else [card]) table

takeCardsFromDeck :: GameState -> GameState
takeCardsFromDeck gameState@(GameState cur def pls _ _ _ _) =
    foldl takeCardsFromDeckForPlayer gameState ((cur:pls) ++ [def])

takeCardsFromDeckForPlayer :: GameState -> Player -> GameState
takeCardsFromDeckForPlayer gameState@(GameState cur def pls ro deck tr table) player@(Player pid name isAi hand)
    |    cur == player = GameState newPlayer def pls ro newDeck tr table
    |    def == player = GameState cur newPlayer pls ro newDeck tr table
    |    otherwise = let playerIndex = fromJust (elemIndex player pls) in
                GameState cur def ((take playerIndex pls) ++ [newPlayer] ++ (drop (playerIndex + 1) pls)) ro newDeck tr table
    where
        newPlayer = Player pid name isAi newHand
        newDeck = drop cardsToTake deck
        newHand = hand ++ (take cardsToTake deck)
        cardsToTake = min 0 (6 - (length hand))


transitCard :: Card -> GameState -> GameState
transitCard card gameState =
    nextPlayer $ nextDefendingPlayer $ putAttackingCardOnTable card gameState

gameOver :: GameState -> Bool
gameOver (GameState cur def pls _ deck _ _) =
    deck == [] && length (filter (\ (Player _ _ _ hand) -> hand /= []) (cur:def:pls)) < 2
