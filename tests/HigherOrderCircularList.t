module HigherOrderCircularList where

import POSIX

root w = class
    env = new posix w
    t := []
    start = action
        env.stdout.write "Tick\n"
        t := (after (sec 1) start) : t
        head t
    result start