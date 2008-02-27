module Primes where

import NoInteraction

primesTo n             = sieve [2..n]
  where sieve (x : xs)
          | x*x > n    = x : xs
          | otherwise  = x : sieve [ y | y <- xs, y `mod` x /= 0 ]

read str = r (reverse str)
 where r (c:cs) = ord c - ord '0' + 10*r cs
       r [] = 0

root = trivial (\(_:a:_) -> show(length(primesTo (read a))))

