module Interactive where

import POSIX

root w = class
    env = new posix w
    init := True
    start = action
        if init then
            env.stdout.write "Hello world!\n"
            after (sec 1) start
    io s = action
        init := False
        if head s == 'q' then
            env.exit 1
        env.stdout.write ("You said: "++s)
    result action
        env.stdin.installR io
        start