module Pointed where

typeclass Pointed a where
  p :: a

instance pointedUnit :: Pointed () where
  p = ()

instance pointedPair :: Pointed (a,b) \\ Pointed a, Pointed b where
  p = (p,p)

instance pointedEitherL :: Pointed (Either a b) \\ Pointed a where
  p = Left p

instance pointedChar :: Pointed Char where
  p = '0'

instance Pointed Int where
  p = 7

data D a = D1 a Char Char 
         | D2
         | D3 Char Char Int

deriving instance pointedD :: Pointed (D a) \\ Pointed a
