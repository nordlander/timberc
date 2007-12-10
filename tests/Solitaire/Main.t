module Main where
 
import Solitaire'CardPile

main = do
  alwaysFalse _ _ = False
  cp <- cardPile alwaysFalse (100,100) translateY
  return ()
