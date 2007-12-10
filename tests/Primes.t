module Primes where



primesTo n       = sieve [2..n]
  where sieve (x : xs)
          | x*x > n = x : xs
          | otherwise = x : sieve [ y | y <- xs, y `mod` x /= 0 ]

