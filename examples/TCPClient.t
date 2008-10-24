module TCPClient where

  import POSIX

  root env = class

     result action
        if size env.argv /= 3 then
           env.stdout.write "Usage: TCPClient host port\n"
           env.exit 0
        host = Host (env.argv!1)
        port = Port (parse (env.argv!2))
        env.stdout.write "Connecting..."
        env.inet.tcp.connect host port (handler env)

private

  handler env sock = class

      rounds := 0 

      neterror err = action 
         env.stdout.write (err ++ "\n")
         env.exit 0

      close       = request 
          env.stdout.write "Server closing; Bye!\n"
          env.exit 0

      receive str = action
          case str of 
            [] -> sock.inFile.close
                  env.stdout.write ("Server closing after "++show rounds++" rounds\n")
                  env.exit 0
                  rounds := rounds+1
            _  -> env.stdout.write (str ++ "\n")

      echo str = action
          sock.outFile.write (init str)
       
      established = action
            env.stdout.write "Hello!\n"
            env.stdin.installR echo
            sock.inFile.installR receive

      result Connection{..}


  init [x] = []
  init (x :xs) = x : init xs
