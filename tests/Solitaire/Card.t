module Solitaire'Card where

data Rank = Ace | King | Queen | Jack | Num Int

data Suit = Spades | Hearts | Diamonds | Clubs

instance Show Rank =
  show Ace     = "Ace"
  show King    = "King"
  show Queen   = "Queen"
  show Jack    = "Jack"
  show (Num n) = "Num " ++ show n

instance Show Suit =
  show Spades   = "Spades"
  show Hearts   = "Hearts"
  show Diamonds = "Diamonds"
  show Clubs    = "Clubs"

type Pos = (Int,Int)

record Card =
  rank :: Rank
  suit :: Suit

  faceUp :: Request Bool
  flip   :: Action
  setPos :: Pos -> Action
  getPos :: Request Pos

instance Show Card =
  show c = show c.rank ++ " " ++ show c.suit

card :: Rank -> Suit -> Pos -> Template Card
card rank suit p = template
  up  := False
  pos := p

  flip     = action up := not up
  faceUp   = request return up

  setPos p = action pos := p
  getPos   = request return pos

  return Card{..}

