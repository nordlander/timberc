module MutualInst where

import POSIX

memoryObj i = class
   m := i
   result 0     

struct Worm where
   i :: Int

wormObj wother  = class
   mem = new memoryObj 1                     
   i = 0    
             
   result Worm {..}

root :: RootType   
root w = class
   env = new posix w
   w1 = new wormObj w2  
   w2 = new wormObj w1 
   result action
     env.stdout.write "OK!\n"
     env.exit 0