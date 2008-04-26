module Output where

import POSIX

root env = class
  str = concat(map f [1000001..1000300 :: Int])
  f x = show x++"\n"
  result struct
     start = action
       env.stdout.write str
       env.exit 0
     io = action
       result ()

