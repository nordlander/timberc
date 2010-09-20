module PreExp where

struct Counter where
  incr  :: Action
  decr  :: Action
  value :: Request Int

counter = class

   n := 0

   incr  = action n := n+1

   decr  = action n := n-1
   
   value = request result n

   result Counter{..}


root w = class
   c = new counter

   g x d = do forall _ <- [1..x] do d.incr
              result <-d.value

   f x = do 
          if  <-c.value > x then
            result <-g (<-c.value) (new counter)
         -- Illegal (locally bound variable y free in preexpression):
         -- result let y = 3 in <-g y (new counter)
          else
            result 3 * <-c.value

   h  = request
           result g 5 c
  
   -- Illegal (lifting preexpression yields request call in class body):
   -- k c = <-c.value
 
   result
     request
       -- Illegal (lifting preexpression causes premature request call):  
       -- k x = <-c.value + x
       -- Illegal (|| is lazy, so right operand should not be evaluated):
       -- if  3>0 || <-c.value > 0 then
       if  <-c.value > 0 then
         result 0
       else
         result <- <-h