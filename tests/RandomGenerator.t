module RandomGenerator where

record Generator a =
  next :: Request a
  
-- Lehmer/Schrage random number generator. Simple and not too bad.
-- baseGen generates integers in the range [0..2^31-1] on 32 bit machines.
-- The n-th number produced is seed * a^n, but with calculations organised to avoid overflow.
-- 7^5 is a primitive root of the prime 2^31 - 1, so this generator has period 2^31 - 2.

baseGen :: Int -> Template (Generator Int)
baseGen seed = template
  a = 16807        -- = 7^5
  m = 2147483647   -- = 2^31 - 1
  q = 127773       -- = m `div` a
  r = 2836         -- = m `mod` a

  state := seed
  
  next = request
    tmp = a * (state `mod` q) - r * (state `div` q)
    state := if tmp > 0 then tmp else tmp + m
    return state

  return Generator{..}