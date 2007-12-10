module Array0 where

listArray = primListArray
sizeArray = primSizeArray

a :: Array Int 
a = listArray [1..10000]

{-
read str = r (reverse str)
 where r (c:cs) = ord c - ord '0' + 10*r cs
       r [] = 0
-}

foldlArray f u a = iter 0 u
  where iter k ack
          | k < sizeArray a = iter (k+1) (f ack (a!k))
          | otherwise = ack

root _ = show (foldlArray (+) 0 a)
