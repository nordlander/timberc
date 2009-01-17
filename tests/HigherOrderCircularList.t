module HigherOrderCircularList where

import POSIX

root env = class
    t := []
    start = action
        env.stdout.write "Tick\n"
        t := (after (sec 1) start) : t
        head t
    result start
