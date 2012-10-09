module Echo3 where

import POSIX

root w = class
   env = new posix w
   current := "Hello!\n"

   save str = action
      current := str

   tick = action
      env.stdout.write current
      after sec 1 send tick

   result action
      env.stdin.installR save
      send tick
