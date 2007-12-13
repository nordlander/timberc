module Test where

import POSIX

root env =
    template
      init := True
      start = action
                if init then
                   env.stdout.write "Hello\n"
                   after (sec 1) start
      io    = action
                init := False
                s <- env.stdin.read
                if head s == 'q' then
                   env.exit 1
                env.stdout.write ("You said: "++s)

      return Prog {..}

