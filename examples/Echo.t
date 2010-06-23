module Echo where

import POSIX

root w = class
   env = new posix w
   
   handler str = action
     env.stdout.write str

   result action
     env.stdin.installR handler
