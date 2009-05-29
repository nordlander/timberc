module Forall where

import POSIX

root env = class
    render xs = do
      forall b <- xs, c <- [1..3] do
        env.stdout.write (show b ++ show c)

    result action
