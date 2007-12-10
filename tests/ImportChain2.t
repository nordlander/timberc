module ImportChain2 where

import ImportChain1

data  A2 > A1 = A2

a2 = A1

f2 x = f1 x

record R2 < R =
  sel2 :: Int

r2 = R2 {sel = 1, sel2 = 3 }