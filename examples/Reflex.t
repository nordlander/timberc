module Reflex where

import POSIX
import RandomGenerator

data State = Idle | Waiting Msg | Counting 


format t = show (secOf t) ++ '.' : fracs ++ " secs"
  where t100  = microsecOf t `div` 10000
        fracs = if t100<10 then '0':show t100 else show t100 

root w = class

   env = new posix w
   gen = new baseGen (microsecOf env.startTime)
   tmr = new timer

   setStatePrint st msg = do
      env.stdout.write (msg ++ "\n")
      state := st

   state := Idle
  
   enter _ = action
      case state of
        Idle ->         waitingTime = sec 2 + millisec (<-gen.next `mod` 2000)
                        msg <- after waitingTime action
                           tmr.reset
                           setStatePrint Counting "Go!!!"
                        setStatePrint (Waiting msg) "Wait..."
                 
        Waiting msg ->  abort msg
                        setStatePrint Idle "Cheat!"

        Counting     -> setStatePrint Idle (format (<- tmr.sample))

   result action
      env.stdin.installR enter
      env.stdout.write "Press return to start\n"
