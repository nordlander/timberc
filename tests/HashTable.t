module HashTable where

struct Dictionary a b where
  insert :: a -> b -> Action
  lookup :: a -> Request (Maybe b)

{-

Wrong design; String should not have ONE hash function.
Better to give hash function as arg to hashDict.

class Hashable a =
  hash :: a -> Int -> Int
  -- requirement: 0 <= hash a n < n

-}

sequ []       = class result []
sequ (x:xs)   = class
                   y = new x
                   ys = new sequ xs
                   result (y:ys)

hashDict hash dictT n = class
  ds = new sequ (replicate n dictT)

  dict = array ds
  
  insert a b = action
    (dict!(hash a n)).insert a b

  lookup a = request
    (dict!(hash a n)).lookup a
   
  result Dictionary {..}

{-
hashDict1 hash dictT n = do
  ds <- sequence (replicate n dictT)  

  dict = array ds
  
  insert a b = (dict!(hash a n)).insert a b

  lookup a = (dict!(hash a n)).lookup a
   
  result Dictionary {..}
-}
