module StringEscapes where

import POSIX

root :: RootType
root w = class
    env = new posix w
    result action
      env.stdout.write ('\"' : '\'' : ";[~\o33\n' \"\\ \HTxy\x000dude"++"\n")
      env.exit 0