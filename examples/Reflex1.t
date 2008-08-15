module Reflex1 where

import POSIX

data State = Idle | Waiting | Counting

root env = class
  
   tmr = new timer

   state   := Idle
   pending := Nothing
  
   press _ = action

      case state of
        Idle ->     env.stdout.write "Wait...\n"
                    msg <- after (sec 2 + millisec 3000) action
                       env.stdout.write "Go!\n"
                       tmr.reset
                       state  := Counting 
                    pending := Just msg
                    state   := Waiting
                 
        Waiting ->  env.stdout.write "Cheat!!!\n"
                    case pending of
                      Just msg -> abort msg
                      Nothing  -> result ()
                    state := Idle

        Counting -> t <- tmr.sample
                    env.stdout.write ("Time is "++format t++" secs\n")
                    state := Idle

   result 
     action
       env.installR env.stdin press
       env.stdout.write "Press return to start\n"

format t = show (secOf t) ++ '.' : fracs
  where 
        t100  = microsecOf t `div` 10000
        fracs = if t100<10 then '0':show t100 else show t100 
