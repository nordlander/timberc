module Test where

import POSIX

root =
    class
      init := True
      start = action
                if init then
                   stdout.write "Hello\n"
                   after (sec 1) start
      io    = action
                init := False
                s <- stdin.read
                if head s == 'q' then
                   exit 1
                stdout.write ("You said: "++s)

      result Prog {..}

