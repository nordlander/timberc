module Dictionary where

  struct Dictionary a b where
    insert :: a -> b -> Request ()
    lookup :: a -> Request (Maybe b)

  listDict :: Class (Dictionary a b) \\ Eq a
  listDict = class
    dict := []
  
    insert a b = request
      dict := insL a b dict

    lookup a = request
      result Prelude.lookup a dict

    result Dictionary {..}


  treeDict :: Class (Dictionary a b) \\ Ord a
  treeDict = class
    dict := Nil
    
    insert a b = request
      dict := insT a b dict

    lookup a = request
      result look a dict

    result Dictionary {..}


  hashDict :: (a -> Int -> Int) -> Class (Dictionary a b) ->
              Int -> Class (Dictionary a b)
  hashDict hash dictC n = class

     ds = new seqC dictC n
     dict = array ds
  
     insert a b = (dict!(hash a n)).insert a b

     lookup a = (dict!(hash a n)).lookup a
   
     result Dictionary {..}

private
  
  insL a b []            = [(a,b)]
  insL a b ((x,y) : xs) 
           | a == x      = (a,b) : xs
           | otherwise   = (x,y) : insL a b xs

  data Tree a = Nil | Node (Tree a) a (Tree a)

  insT a b Nil                 = Node Nil (a,b) Nil
  insT a b (Node l (x,y) r)  
         | a == x              = Node l (a,b) r
         | a < x               = Node (insT a b l) (x,y) r
         | a > x               = Node l (x,y) (insT a b r)

  look a Nil                   = Nothing
  look a (Node l (x,y) r)     
          | a == x             = Just y
          | a < x              = look a l
          | a > x              = look a r

  seqC c 0       = class result []
  seqC c n       = class
                     x  = new c
                     xs = new seqC c (n-1)
                     result (x:xs)
