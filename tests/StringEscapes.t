module StringEscapes where

import POSIX

root env = class result action env.stdout.write ('\"' : '\'' : ";[~\o33\n' \"\\ \HT")

