module Defaulting where

import POSIX

--y :: [Int]
y = 7:8:y

root :: RootType
root w = class
    env = new posix w
    x = show (head (tail y))
    result action
      env.stdout.write (x ++ "\n")
      env.exit 0