module Data'Functional'List where

delete             :: a -> [a] -> [a] \\ Eq a
delete              = deleteBy (==)

deleteBy           :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []    = []
deleteBy eq x (y:ys)= if x `eq` y then ys else y:deleteBy eq x ys


(\\)               :: [a] -> [a] -> [a] \\ Eq a
(\\)                = foldl (flip delete)

elemBy, notElemBy  :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq _ []      = False
elemBy eq x (y:ys)  = x `eq` y || elemBy eq x ys

notElemBy eq x xs   = not (elemBy eq x xs)

lookupBy           :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq key []  = Nothing
lookupBy eq key ((x,y):xys)
    | key `eq` x    = Just y
    | otherwise     = lookupBy eq key xys

partition          :: (a -> Bool) -> [a] -> ([a],[a])
partition p         = foldr select ([],[])
		      where select x (ts,fs) | p x       = (x:ts,fs)
		  			     | otherwise = (ts,x:fs)

nub                :: [a] -> [a] \\ Eq a
nub                 = nubBy (==)

nubBy              :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []         = []
nubBy eq (x:xs)     = x : nubBy eq (filter (\y -> not (eq x y)) xs)

sort               :: [a] -> [a] \\ Ord a
sort []             = []
sort (x : xs)       = sort small ++ x : sort big
  where (small,big) = partition (\y -> y <= x) xs
