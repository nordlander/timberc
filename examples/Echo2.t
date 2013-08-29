module Echo2 where

import POSIX

root w = class
   env = new posix w
   count := 1

   echoPrompt str = do
      env.stdout.write (str ++ show count ++ "> ")
      count := count+1

   result action 
      env.stdin.installR (\s -> action echoPrompt s)
      echoPrompt "Welcome to Echo2!\n"


