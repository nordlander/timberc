module PreExp where

import Counter



root w = class
   c = new counter

   g x d = do forall _ <- [1..x] do d.incr
              result <-d.value

   f x = do 
          if  <-c.value > x then
            result <-g (<-c.value) (new counter)
         -- Both the following illegal:
         -- result let y = <-g.value in <-g y (new counter)
         -- result let y = 3 in <-g y (new counter)
          else
            result 3 * <-c.value

   h  = request
           result g 5 c
  
   -- Illegal:  
   -- k c = <-c.value

   result
     request
       if  <-c.value > 0 then
         result 0
       else
         result <- <-h