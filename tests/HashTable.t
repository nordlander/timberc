module HashTable where

record Dictionary a b =
  insert :: a -> b -> Action
  lookup :: a -> Request (Maybe b)

{-

Wrong design; String should not have ONE hash function.
Better to give hash function as arg to hashDict.

class Hashable a =
  hash :: a -> Int -> Int
  -- requirement: 0 <= hash a n < n

-}

hashDict hash dictT n = template
  ds <- sequence (replicate n dictT)

  dict = array ds
  
  insert a b = action
    (dict!(hash a n)).insert a b

  lookup a = request
    (dict!(hash a n)).lookup a
   
  return Dictionary {..}

{-
hashDict1 hash dictT n = do
  ds <- sequence (replicate n dictT)  

  dict = array ds
  
  insert a b = (dict!(hash a n)).insert a b

  lookup a = (dict!(hash a n)).lookup a
   
  return Dictionary {..}
-}
