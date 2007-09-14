module Prelude where


class Num a =
 (+),(-),(*) :: a -> a -> a
 negate, abs :: a -> a
 fromInt :: Int -> a

instance Num Int =
  (+) = primIntPlus
  (-) = primIntMinus
  (*) = primIntTimes
  negate = primIntNeg
  abs x 
   | x < 0     = primIntNeg x
   | otherwise = x
  fromInt n = n

instance Num Float =
  (+) = primFloatPlus
  (-) = primFloatMinus
  (*) = primFloatTimes
  negate = primFloatNeg
  abs x
   | x < 0.0   = primFloatNeg x
   | otherwise = x
  fromInt = primIntToFloat

-- default Num Int < Num Float

class Eq a =
  (==),(/=) :: a -> a -> Bool

instance Eq PID =
  (==) = primPidEQ
  (/=) = primPidNE

instance Eq Char =
  a == b = ord a == ord b
  a /= b = ord a /= ord b 

instance Eq [a] \\ Eq a =
  [] == []              = True
  a : as == b : bs      = a == b && as == bs
  _ == _                = False
  xs /= ys              = not ( xs == ys)

class Ord a < Eq a =
  (<),(<=),(>),(>=) :: a -> a -> Bool

instance Ord Int =
  (==) = primIntEQ
  (/=) = primIntNE
  (<)  = primIntLT
  (<=) = primIntLE
  (>)  = primIntGT
  (>=) = primIntGE

instance Ord Float =
  (==) = primFloatEQ
  (/=) = primFloatNE
  (<)  = primFloatLT
  (<=) = primFloatLE
  (>)  = primFloatGT
  (>=) = primFloatGE

instance Ord Time =
  (==) = primTimeEQ
  (/=) = primTimeNE
  (<)  = primTimeLT
  (<=) = primTimeLE
  (>)  = primTimeGT
  (>=) = primTimeGE

class Show a =
  show :: a -> String

instance Show Int =
  show 0            = "0"
  show n
    |n < 0          = '-' : show (negate n)
    | otherwise     = reverse (digs n)
    where dig n     = chr (n + ord '0')
          digs n
           | n < 10    = [dig n]
           | otherwise = dig (n `mod` 10) : digs (n `div` 10)
 
instance Show Float =
  show x 
    | x < 0                  = '-' : show (0.0-x)
    | x > 0.1 && x < 1000000 = let is = show (floor x)
                                   dl = 6 - length is
                                   ds0 = show(floor ((x - fromInt (floor x)) * 10 ^ dl))
                                   ds = replicate (dl - length ds0) '0' ++ ds0 
                               in  is ++ '.' : ds
   | otherwise               = "Show Float is incomplete"
 
instance Show Bool =
  show False = "False"
  show True  = "True"

instance Show Char =
  show c = [c]

instance Show (Maybe a) \\ Show a =
  show Nothing      = "Nothing"
  show (Just x)     = "Just (" ++ show x ++ ")"

instance Show (Maybe Bool) =
  show _ = "mb"

instance Show String =
  show s = '"' : s ++ "\""

instance Show [a] \\ Show a =
  show [] = "[]"
  show (x : xs) = '[' : show x ++ concat (map (\x -> ',' : show x) xs) ++ "]"

data Maybe a = Just a | Nothing

type String         = [Char]

head               :: [a] -> a
head (x : _)        = x

tail               :: [a] -> [a]
tail (_ : xs)       = xs

length             :: [a] -> Int
length []           = 0
length (_ : xs)     = 1 + length xs

reverse            :: [a] -> [a]
reverse xs          = rev xs []
  where rev [] ys   = ys
        rev (x : xs) ys
                    = rev xs (x : ys)

map                :: (a -> b) -> [a] -> [b]
map f []            = []
map f (x : xs)      = f x : map f xs

replicate n x
  | n <= 0          = []
  | otherwise       = x : replicate (n-1) x

filter             :: (a -> Bool) -> [a] -> [a]
filter p []         = []
filter p (x : xs) 
    | p x           = x : filter p xs
    | otherwise     = filter p xs


foldr              :: ( a -> b -> b) -> b -> [a] -> b
foldr f u []        = u
foldr f u (x : xs)  = f x (foldr f u xs)

foldl              :: (a -> b -> a) -> a -> [b] -> a
foldl f u []        = u 
foldl f u (x : xs)  = foldl f (f u x) xs

concat              = foldr (++) []

fromTo m n
       | m > n      = []
       | otherwise  = m : fromTo (m+1) n

zip (x:xs) (y:ys)   = (x,y) : zip xs ys
zip _ _             = []

elem               :: a -> [a] -> Bool \\ Eq a
elem x []           = False
elem x (y : ys)     = x == y || elem x ys

lookup              :: a -> [(a,b)] -> Maybe b \\ Eq a
lookup x []          = Nothing
lookup x ((a,b) : xs) 
     | x == a        = Just b
     | otherwise     = lookup x xs

take               :: Int -> [a] -> [a]
take 0 xs           = xs
take n []           = []
take n (x : xs) 
        | n > 0     = take (n-1) xs

(++)               :: [a] -> [a] -> [a] 
[] ++ ys            = ys
(x:xs) ++ ys        = x : xs ++ ys

flip               :: (a -> b -> c) -> b -> a -> c
flip f x y          = f y x

curry              :: ((a,b) -> c) -> a -> b -> c 
curry f x y         = f (x,y)

uncurry            :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y)     = f x y

not                :: Bool -> Bool
not True            = False
not False           = True

otherwise          :: Bool
otherwise           = True

ord                :: Char -> Int
ord                 = primCharToInt

chr                :: Int -> Char
chr                 = primIntToChar

a ^ 0               = 1
a ^ n 
  | even n          = (a * a) ^ (n `div` 2)
  | otherwise       = a * (a * a) ^ (n `div` 2)

div, mod           :: Int -> Int -> Int
div                 = primIntDiv
mod                 = primIntMod

even, odd          :: Int -> Bool
even x              = x `mod` 2 == 0
odd x               = x `mod` 2 == 1

gcd                :: Int -> Int -> Int
gcd x y             = gcd' (abs x) (abs y)
  where gcd' a 0
         | a > 0    = a
        gcd' a b    = gcd' b (a `mod` b)


max, min           :: a -> a -> a \\ Ord a
max x y
  | x >= y          = x
  | otherwise       = y

min x y
  | x <= y          = x
  | otherwise       = y

floor               = primFloatToInt

round x             = floor (x + 0.5)

record Point = 
  x,y :: Int

type P = Point

x = 3
