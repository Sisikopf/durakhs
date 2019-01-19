module Durak.GameLogic
    (startGame) where

import Durak.Models
import Durak.UI
import Durak.Utils
import Data.Maybe
import Data.List

cardsInHand :: Int
cardsInHand = 6

startState :: GameState
startState =
    (GameState currentPlayer defendingPlayer otherPlayers 1 readyDeck trump [])
    where
        readyDeck = tail deckAfterGiveaway ++ [head deckAfterGiveaway]
        trump = getSuit $ head deckAfterGiveaway
        deckAfterGiveaway = drop (4*cardsInHand) fullDeck
        currentPlayer = Player 0 "You" False (take cardsInHand fullDeck)
        defendingPlayer = Player 1 "Trus" True (take cardsInHand (drop cardsInHand fullDeck))
        otherPlayers = [
            Player 2 "Balbes" True (take cardsInHand (drop (2*cardsInHand) fullDeck)),
            Player 3 "Byvaliy" True (take cardsInHand (drop (3*cardsInHand) fullDeck))]
        fullDeck = [Card rank suit | rank <- [Eight .. Ace], suit <- [Clubs .. Spades]]

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


startGame :: IO ()
startGame = do
   mainCycle startState

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
        then return $ GameState cur def pls (ro + 1) deck tr table
        else do
            action <- if allCardsUncovered gameState
                then askForCoverTakeOrTransitCards gameState
                else askForCoverOrTakeCards gameState
            return (case action of
                Take -> nextCurrentPlayer $ nextDefendingPlayer $ takeCardsFromTable gameState
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
                Take -> return $ nextCurrentPlayer $ nextDefendingPlayer $ takeCardsFromTable gameState
                Cover card -> continueDefendingMove (putDefendingCardOnTable card gameState)

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
            (GameState cur def (filter (\ (Player _ _ _ hand) -> hand /= []) pls) ro deck trump [])
        (GameState cur def pls ro deck trump table) = nextDefendingPlayer $ takeCardsFromDeck $ gameState

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
    foldl takeCardsFromDeckForPlayer gameState (pls ++ [def] ++ [cur])

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
        cardsToTake = max 0 (cardsInHand - (length hand))


transitCard :: Card -> GameState -> GameState
transitCard card gameState =
    nextCurrentPlayer $ nextDefendingPlayer $ putAttackingCardOnTable card gameState

gameOver :: GameState -> Bool
gameOver (GameState cur def pls _ deck _ _) =
    deck == [] && length (filter (\ (Player _ _ _ hand) -> hand /= []) (cur:def:pls)) < 2
