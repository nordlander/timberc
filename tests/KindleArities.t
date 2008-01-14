module VTest where

import POSIX

record Counter =
        display :: Action

acts = map (.display) []


test True  = template
               return (\y->y) 
test False = template
               return (\_->action return ())
