module Reflex0 where

import POSIX
import Timer

format n = show secs ++ '.' : fracs
  where secs  = n `div` 100 
        n100  = n `mod` 100
        fracs = if n100<10 then '0':show n100 else show n100 

root env = template
  t <- timer
  start = action
     env.stdout.write "Wait...\n"
     after (sec 2) action
        env.stdout.write "Go!\n"
        t.startT
  io = action
     env.stdin.read
     t.stop
     n <- t.readT
     t.reset
     env.stdout.write ("Time is "++format n++" secs\n")
     start

  return Prog{..}