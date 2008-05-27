module Dictionary where

  struct Dictionary a b where
    insert :: a -> b -> Action
    lookup :: a -> Request (Maybe b)

  listDict :: Class (Dictionary a b) \\ Eq a
  listDict = class
    dict := []
  
    insert a b = action
      dict := ins a b dict

    lookup a = request
      result Prelude.lookup a dict

    result Dictionary {..}


  treeDict :: Class (Dictionary a b) \\ Ord a
  treeDict = class
    dict := Nil
    
    insert a b = action
      dict := ins' a b dict

    lookup a = request
      result look' a dict

    result Dictionary {..}

private
  
  ins a b []            = [(a,b)]
  ins a b ((x,y) : xs) 
           | a == x     = (a,b) : xs
           | otherwise  = (x,y) : ins a b xs

  data Tree a = Nil | Node (Tree a) a (Tree a)

  ins' a b Nil                 = Node Nil (a,b) Nil
  ins' a b (Node l (x,y) r)  
          | a == x             = Node l (a,b) r
          | a < x              = Node (ins' a b l) (x,y) r
          | a > x              = Node l (x,y) (ins' a b r)

  look' a Nil                  = Nothing
  look' a (Node l (x,y) r)     
          | a == x             = Just y
          | a < x              = look' a l
          | a > x              = look' a r
