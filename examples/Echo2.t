module Echo2 where

import POSIX

root w = class
   env = new posix w
   count := 1

   prompt = do
      env.stdout.write (show count++"> ")
      count := count+1

   handler str = action
      env.stdout.write str
      prompt

   result action 
      env.stdin.installR handler
      env.stdout.write "Welcome to Echo2!\n"
      prompt

