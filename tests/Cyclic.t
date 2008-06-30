module Cyclic where

import POSIX

struct S where
    v :: Int
    r :: S
    
s1 = { v = 3, r = s3 }
f 0 = { v = 6, r = s1 }
f n = f (n-1)
s3 = f 2


root env =
    class
       s := s1
       io _ = action
               s := s.r
               env.stdout.write (show s.v)
       result action
         env.installR env.stdin io
         env.stdout.write (show s1.v)

ggg n = let apa = 'a':bepa
            bepa = 'b':cepa
            cepa = 'c':apa
        in apa
