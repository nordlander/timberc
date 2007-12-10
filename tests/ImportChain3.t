module ImportChain3 where

import ImportChain1
import ImportChain2

f3 :: a -> A1'ImportChain1     -- OK also to give the type signature; ImportChain2 reexport imported types
f3 x = f2 x                    -- OK; type A1'ImportChain1 in scope

a3 = A1'ImportChain1           -- OK; constructor A1 in scope in qualified form

r3 = R2 { sel'ImportChain1 = 3, sel2 = 4} -- OK.

-- a3 = A1                      -- illegal; constructor A1 not in scope unqualified 

-- f3 x = f1'ImportChain1 x    -- Illegal; f1 not in scope

