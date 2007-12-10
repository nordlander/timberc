module ImportChain1 where

data A1 = A1

f1 x = A1

record R = 
  sel :: Int

r = R { sel = 1 }