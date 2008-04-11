module Data'Objects'Stack where

struct Stack a where 
  push :: a -> Action
  pop  :: Action
  top  :: Request a
  size :: Request Int

stk = class
   xs := []
   
   push x = action
     xs := x : xs

   pop = action
     xs := tail xs
 
   top = request
     result (head xs)

   size = request
     result (length xs)

   result Stack{..}

