module Reflex0 where

import POSIX
use Timer

format n = show secs ++ '.' : fracs
  where secs  = n `div` 100 
        n100  = n `mod` 100
        fracs = if n100<10 then '0':show n100 else show n100 

root env = class
  t = new Timer.timer
  press _ = action
     t.Timer.stop
     n <- t.Timer.read
     t.Timer.reset
     env.stdout.write ("Time is "++format n++" secs\n")
     start


  start = action
     env.stdout.write "Wait...\n"
     msg <- after (sec 2) action
        env.stdout.write "Go!\n"
        t.Timer.start

  result start
