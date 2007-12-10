module Solitaire'CardPile where

import Data'Objects'Stack
import Solitaire'Card

record CardPile < Stack Card =
  canTake :: Card -> Request Bool

translateY :: Pos -> Int -> Pos
translateY (x,y) dy = (x,y+dy)

cardPile :: (Card -> Card -> Bool) -> Pos -> (Pos -> Int -> Pos) -> Template CardPile
cardPile ct p relPos = template

  Stack {push = push0 ..} <- stk

  push c = action
    sz <- size
    push0 c
    c.setPos (relPos p sz)

  canTake c = request
    t <- top
    return (ct c t)

  return CardPile{..}

