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

  handler env peer = class
       deliver str = action 
          env.stdout.write (str ++ "\n")
       established = action 
          env.stdout.write "\n"
          env.stdin.installR (peer.deliver @ init)
       close       = action 
          env.stdout.write "Server closing; Bye!\n"
          env.exit 0
       neterror err = action 
          env.stdout.write (err ++ "\n")
          env.exit 0
       result Connection {..}

  init [x] = []
  init (x :xs) = x : init xs
