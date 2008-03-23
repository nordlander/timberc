module Array0 where


a :: Array Int 
a = array [1..10000]

foldlArray f u a = iter 0 u
  where iter k ack
          | k < size a = iter (k+1) (f ack (a!k))
          | otherwise = ack

root = show (foldlArray (+) 0 a)
