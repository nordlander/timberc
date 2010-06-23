module Output where

import POSIX

root :: RootType
root w = class
  env = new posix w
  str = concat(map f [1000001..1000300 :: Int])
  f x = show x ++ "\n"
  result action
    n <- env.stdout.write str
    env.stdout.write ("\n" ++ show n ++ " chars written\n")
    env.exit 0

