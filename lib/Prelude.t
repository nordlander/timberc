module Prelude where

implicit struct IntLiteral a where
 fromInt :: Int -> a

implicit intInt :: IntLiteral Int
intInt = struct
  fromInt n = n
  
implicit intFloat ::IntLiteral Float
intFloat = struct
  fromInt = primIntToFloat
  
implicit struct Num a where
 (+),(-),(*) :: a -> a -> a
 negate :: a -> a

implicit numInt :: Num Int
numInt = struct 
  (+) = primIntPlus
  (-) = primIntMinus
  (*) = primIntTimes
  negate = primIntNeg

implicit numFloat :: Num Float 
numFloat = struct
  (+) = primFloatPlus
  (-) = primFloatMinus
  (*) = primFloatTimes
  negate = primFloatNeg

default intInt < intFloat

implicit struct Eq a where
  (==),(/=) :: a -> a -> Bool

implicit eqInt :: Eq Int
eqInt = struct
  (==) = primIntEQ
  (/=) = primIntNE

implicit eqFloat :: Eq Float
eqFloat = struct
  (==) = primFloatEQ
  (/=) = primFloatNE

implicit eqTime :: Eq Time
eqTime = struct
  (==) = primTimeEQ
  (/=) = primTimeNE

implicit eqPID :: Eq PID
eqPID = struct
  (==) = primPidEQ
  (/=) = primPidNE

implicit eqChar :: Eq Char
eqChar = struct
  a == b = ord a == ord b
  a /= b = ord a /= ord b

implicit eqUnit :: Eq ()
eqUnit = struct
  _ == _ = True
  _ /= _ = False

implicit eqList :: Eq [a] \\ Eq a
eqList = struct
    [] == []              = True
    a : as == b : bs  = a == b && as == bs
    _ == _               = False
    xs /= ys             = not ( xs == ys)

implicit eqEither :: Eq (Either a b) \\ Eq a, Eq b
eqEither = struct
  Left x  == Left y  = x == y
  Right x == Right y = x == y
  _       == _       = False
  x       /= y       = not (x == y)

implicit eqPair :: Eq (a,b) \\ Eq a, Eq b
eqPair = struct
  (a,b) == (c,d) = a==c && b==d
  x /= y = not (x==y)

implicit struct Ord a < Eq a where
  (<),(<=),(>),(>=) :: a -> a -> Bool

implicit ordInt :: Ord Int
ordInt = Ord {..}
  where Eq {..} = eqInt
        (<)  = primIntLT
        (<=) = primIntLE
        (>)  = primIntGT
        (>=) = primIntGE

implicit ordFloat :: Ord Float
ordFloat = Ord {..}
  where Eq {..} = eqFloat
        (<)  = primFloatLT
        (<=) = primFloatLE
        (>)  = primFloatGT
        (>=) = primFloatGE

implicit ordTime :: Ord Time
ordTime = Ord {..}
  where Eq {..} = eqTime
        (<)  = primTimeLT
        (<=) = primTimeLE
        (>)  = primTimeGT
        (>=) = primTimeGE

implicit ordUnit :: Ord ()
ordUnit = Ord{..}
  where Eq{..} = eqUnit
        _ < _  = False
        _ <= _ = True
        _ > _  = False
        _ >= _ = True
    
     
implicit struct Show a where
  show :: a -> String

implicit showInt :: Show Int
showInt = struct
  show 0            = "0"
  show n
    |n < 0          = '-' : show (negate n)
    | otherwise     = reverse (digs n)
    where dig n     = chr (n + ord '0')
          digs n
           | n < 10    = [dig n]
           | otherwise = dig (n `mod` 10) : digs (n `div` 10)
 
implicit showFloat :: Show Float
showFloat = struct
   show x 
    | x < 0                  = '-' : show (0.0-x)
    | x > 0.1 && x < 1000000 = let is = show (floor x)
                                   dl = 6 - length is
                                   ds0 = show(floor ((x - fromInt (floor x)) * 10 ^ dl))
                                   ds = replicate (dl - length ds0) '0' ++ ds0 
                               in  is ++ '.' : ds
    | otherwise               = "Show Float is incomplete"

implicit showBool :: Show Bool
showBool = struct
  show False = "False"
  show True  = "True"

implicit showChar :: Show Char
showChar = struct
  show c = [c]

implicit showMaybe :: Show (Maybe a) \\ Show a
showMaybe = struct
  show Nothing      = "Nothing"
  show (Just x)     = "Just (" ++ show x ++ ")"

implicit showString :: Show String
showString = struct
  show s = '"' : s ++ "\""

implicit showList :: Show [a] \\ Show a
showList = struct
  show [] = "[]"
  show (x : xs) = '[' : show x ++ concat (map (\x -> ',' : show x) xs) ++ "]"

implicit struct Parse a where
  parse :: String -> a

implicit parseInt :: Parse Int
parseInt = struct
  parse str = r (reverse str)
    where r (c:cs) = ord c - ord '0' + 10*r cs
          r [] = 0


implicit struct Enum a where
  fromEnum :: a -> Int
  toEnum :: Int -> a
--  enumFromThenTo :: a -> a -> a -> [a]

implicit enumInt :: Enum Int
enumInt =struct
  fromEnum n = n
  toEnum n = n
--  enumFromTo = fromToInt
{-
  enumFromThenTo a b c
    | signum (b-a) == signum (c-a) = a : enumFromThenTo b (2*b - a) c
    | a==c                         = [a]
    | otherwise                    = []
    where signum x
            | x > 0 = 1
            | x == 0 = 0
            | x < 0 = -1
-}

implicit enumChar :: Enum Char
enumChar = struct
   fromEnum = primCharToInt
   toEnum = primIntToChar
--   enumFromTo a b = map primIntToChar (enumFromTo (primCharToInt a) (primCharToInt b))
{-
   enumFromThenTo a b c = map chr (enumFromThenTo (ord a) (ord b) (ord c))
-}
forallDo f []       = do result ()
forallDo f (x : xs) = do f x
                         forallDo f xs


data Maybe a = Just a | Nothing

type String         = [Char]

enumFromTo :: a -> a -> [a] \\ Enum a
enumFromTo a b = map toEnum (fromToInt (fromEnum a) (fromEnum b))

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

replicate :: Int -> a -> [a]
replicate n x
  | n < 0          = []
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

fromToInt :: Int -> Int -> [Int]
fromToInt m n
       | m > n      = []
       | otherwise  = m : fromToInt (m+1) n

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

take, drop          :: Int -> [a] -> [a]
take 0 xs           = []
take n []           = []
take n (x : xs) 
        | n > 0     = x : take (n-1) xs

drop 0 xs           = xs
drop n []           = []
drop n (x : xs)
       | n > 0      = drop (n-1) xs
         
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

abs x  = case x < 0 of
             True  -> -x
             False -> x


max, min           :: a -> a -> a \\ Ord a
max x y
  | x >= y          = x
  | otherwise       = y

min x y
  | x <= y          = x
  | otherwise       = y

floor               = primFloatToInt

round x             = floor (x + 0.5)

sequence []         = do result []
sequence (x : xs)   = do a  <- x
                         as <- sequence xs
                         result a : as

mapM f []           = do result []
mapM f (x : xs)     = do a <- f x
                         as <- mapM f xs
                         result a : as

struct Point where 
   x,y :: Int

f @ g               = \x -> f (g x)
