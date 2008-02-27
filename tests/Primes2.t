module Primes2 where

import POSIX 

root env = class

   limit = read (head (tail env.argv))
   primesBound = limit `div` log3 limit

   primes := uniarray primesBound 0    --enough for primes < limit
   top := -1

   test k = loop 0
      where loop n = do 
              p = primes!n
              if p*p > k then
                 result True
              elsif k `mod` p  == 0 then
                 result False
              else loop (n+1)

   tryFrom k = do
     p <- test k
     if p then 
        top := top + 1
        primes!top := k
     if k < limit then tryFrom (k+1)

   start = action
     env.stdout.write ("Assigned array with "++show primesBound++" elements\n")
     top := 0
     primes!0 := 2
     tryFrom 3
     env.stdout.write (show (top+1)++"\n")
     env.exit 0
   io = action
     result ()
   result Prog {..}          

read str        = r (reverse str)
 where r (c:cs) = ord c - ord '0' + 10*r cs
       r []     = 0


log3 n  
  | n < 3       = 0
  | otherwise   = 1 + log3 (n `div` 3)
