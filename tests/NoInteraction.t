module NoInteraction where

import POSIX

trivial f env = class
  start = action
    env.stdout.write (f env.argv++"\n")
    env.exit 0
  io = action
    result ()
  result Prog{..}
