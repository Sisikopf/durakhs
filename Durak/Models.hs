module Durak.Models
    ( Rank(..)
    , Suit(..)
    , Trump(..)
    , Card(..)
    , CardPair(..)
    , Hand(..)
    , Deck(..)
    , Player(..)
    , Table(..)
    , GameState(..)
    , getPlayerId
    , getPlayerName
    ) where

data Rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
instance Show Rank where
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

data Suit = Clubs | Diamonds | Hearts | Spades
instance Show Suit where
    show Clubs = "♣"
    show Diamonds = "♦"
    show Hearts = "♥"
    show Spades = "♠"

type Trump = Suit
data Card = Card Rank Suit
instance Show Card where
    show (Card rank suit) = show rank ++ (show suit)
    showList (card:[]) = (\y -> show card)
    showList (card:cards) = (\y -> show card ++ " " ++ (show cards))

type Hand = [Card]
type Deck = [Card]
data Player = Human Int String Hand | AI Int String Hand
getPlayerId (Human id _ _) = id
getPlayerId (AI id _ _) = id
getPlayerName (Human _ name _) = name
getPlayerName (AI _ name _) = name
data CardPair = CardPair Card (Maybe Card)
instance Show CardPair where
    show (CardPair card Nothing) = show card
    show (CardPair card (Just anotherCard)) = show card ++ "/" ++ (show anotherCard)
    showList (cardPair:[]) = (\y -> show cardPair)
    showList (cardPair:cardPairs) = (\y -> show cardPair ++ " " ++ (show cardPairs))

type Table = [CardPair]
data GameState = GameState {
                    currentPlayerId :: Int,
                    defendingPlayerId :: Int,
                    roundNum :: Int,
                    players :: [Player],
                    deck :: Deck,
                    trump :: Trump,
                    table :: Table
                 }
