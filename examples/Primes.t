module Primes2 where

import POSIX 

root env = class
   limit :: Int
   limit = parse (env.argv!1)
   primesBound = limit `div` log3 limit

   primes := uniarray primesBound 0
   count  := 0

   isPrime k = loop 0
      where loop n = do 
              p = primes!n
              if p*p > k then
                 result True
              elsif k `mod` p  == 0 then
                 result False
              else loop (n+1)

   tryFrom k = do
     p <- isPrime k
     if p then 
        count := count + 1
        primes!count := k
     if k < limit then tryFrom (k+1)

   result action
     primes!0 := 2
     tryFrom 3
     env.stdout.write (show (count+1)++"\n")
     env.exit 0


log3 :: Int -> Int
log3 n  
  | n < 3       = 0
  | otherwise   = 1 + log3 (n `div` 3)