module StressTest where

import POSIX
import Data.Functional.List

port = Port 12345 

check env k peer = class
    n := 1
    deliver str = action
        env.stdout.write (show k ++ ", mess " ++ show n ++ " >>> " ++ str ++ "\n")
        if n < 50 then
          after (millisec 100) peer.deliver "Hi!"
        else
          env.stdout.write (show k ++ " done!!")
          peer.close
        n := n+1
          
    established = peer.deliver "Hi!"

    neterror str = action
          env.stdout.write (show k ++ " error: " ++ str)

    close = action result ()

    result Connection {..}
 

root env = class

    args = [1..parse (env.argv!1)] 

    finish = action 
       env.stdout.write "Done!\n"
       env.exit 0
    
    result action
       forall i <- args do
          env.inet.tcp.connect (Host "localhost") port (check env i)
       after (sec 20) finish 
