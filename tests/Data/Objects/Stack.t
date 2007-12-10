module Data'Objects'Stack where

record Stack a = 
  push :: a -> Action
  pop  :: Action
  top  :: Request a
  size :: Request Int

stk = template
   xs := []
   
   push x = action
     xs := x : xs

   pop = action
     xs := tail xs
 
   top = request
     return (head xs)

   size = request
     return (length xs)

   return Stack{..}

