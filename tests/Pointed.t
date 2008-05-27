module Pointed where

implicit struct Pointed a where
  p :: a

implicit pointedUnit :: Pointed ()
pointedUnit = { p = () }

implicit pointedPair :: Pointed (a,b) \\ Pointed a, Pointed b
pointedPair = struct
  p = (p,p)

implicit pointedEitherL :: Pointed (Either a b) \\ Pointed a
pointedEitherL = struct
  p = Left p

implicit pointedChar :: Pointed Char
pointedChar = { p = '0' }

data D a = D1 a Char Char 
         | D2
         | D3 Char Char Int

default pointedD :: Pointed (D a) \\ Pointed a

