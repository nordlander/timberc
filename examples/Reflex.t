module Reflex where

import POSIX

data State = Idle | Waiting Msg | Counting

root env = class
  
   print str = env.stdout.write (str ++ "\n")

   tmr = new timer

   state := Idle
  
   enter _ = action
      case state of
        Idle ->         msg <- after (sec 2) action
                           tmr.reset
                           print "Go!"
                           state := Counting 
                        print "Wait..."
                        state := Waiting msg
                 
        Waiting msg ->  abort msg
                        print "Cheat!!!"
                        state := Idle

        Counting ->     t <- tmr.sample
                        print (format t)
                        state := Idle

   result 
     action
       env.stdin.installR enter
       print "Press return to start"

format t = show (secOf t) ++ '.' : fracs ++ " secs"
  where t100  = microsecOf t `div` 10000
        fracs = if t100<10 then '0':show t100 else show t100 
