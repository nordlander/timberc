module File where

import POSIX

root env = class
   result struct
     start = action
        fname = head (tail env.argv)
        f <- env.open fname Read
        f2 <- env.open (fname ++ "2") Write
        inp <- f.read
        f2.write inp
        env.stdout.write inp
        env.exit 0
     io = action
        result ()
