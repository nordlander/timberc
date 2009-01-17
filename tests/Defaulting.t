module Defaulting where

import POSIX

--y :: [Int]
y = 7:8:y

root env = class result action
                    x = show (head (tail y))
                    env.stdout.write x
