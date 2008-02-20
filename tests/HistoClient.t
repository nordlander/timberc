module HistoClient where

import POSIX
import Histo


root env = class
             bds :: Array Int
             bds = array [10, 20, 30]
             h = new histo bds

             start = action
                env.stdout.write "Hej\n"

             io = action
                d <- env.stdin.read
                if head d == 'q' then
                   rs <- h.getResult
                   showResults rs
                   env.exit 0
                else
                   h.addObs(read d)
                   result ()

             showResults rs = do
                   forall i <- [0..size bds-1] do
                     env.stdout.write ("   -- "++show(bds!i))
                     env.stdout.write ("  " ++ show (rs!i)++"\n")
                   env.stdout.write ("   >  "++show(bds!(size bds - 1)))
                   env.stdout.write ("  " ++ show (rs!(size bds))++"\n")
                  

             result Prog{..}


-- This removes the newline at the end of input strings!!!
read str = r (tail(reverse str))
 where r (c:cs) = ord c - ord '0' + 10 * r cs
       r [] = 0

