module EchoServer where

import POSIX

port = Port 12345

server log peer = class
   n := 1
   p = show peer.host
   result struct
            deliver str  = action
                             log (p++": "++str)
                             peer.deliver (show n ++"> "++str)
                             n := n+1
            established  = log ("Connected from "++p)
            neterror str = log ("Neterror: "++str)
            close        = log (p ++ " closing")

root env = class
            log str = action
               env.stdout.write ('[':str ++ "]\n")
            result action
               cl <- env.inet.tcp.listen port (server log)
               after (sec 500) action
                                 env.stdout.write "Closing...\n"
                                 cl.close
                                 env.exit 0


