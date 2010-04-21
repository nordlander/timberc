module Echo where

import POSIX

echo env = class
              handler str = action
                   env.stdout.write str
              result handler

root :: World -> Cmd () ()
root w = do
   env = new posix w
   handler = new echo env
   env.stdin.installR handler

