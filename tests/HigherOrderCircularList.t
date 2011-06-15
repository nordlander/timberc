module HigherOrderCircularList where

import POSIX

root w = class
    env = new posix w
    t := []
    start = action
        env.stdout.write "Tick\n"
        t := start : t
        after (sec 1) send head t
    result start