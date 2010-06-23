module LocalSig where

import POSIX

root w = class
            env = new posix w
            result action
              print :: a->Action
              print s = action
                              env.stdout.write ("OK\n")
                              env.exit 0
              print False