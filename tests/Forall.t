module Forall where

import POSIX

struct Cell where
  depth :: Request Int
  setNb :: Cell -> Request ()

cell = class
          nb := Nothing
  
          depth = request
                    case nb of
                       Nothing -> result 1
                       Just c -> result 1 + <- c.depth

          setNb c = request nb := Just c

          result Cell{..}

root w = class
  result action
   env = new posix w
   x : xs = forall _ <- [1..8000] new cell 
   forall (c,l) <- zip (x : xs) xs do c.setNb l
   env.stdout.write (show (<- x.depth) ++ " cells linked\n")
   zs = forall z <- [1..10] new class
          result request result z
   env.stdout.write (show (<- last zs) ++ "\n")
   ws <- forall n <- [9..11] do
       result <-env.stdout.write ("Hej " ++ show n ++ "\n")
   env.stdout.write (show ws ++ "\n")  
   rs <- forall q <- take 4 xs do
      result <- q.depth
   env.stdout.write (show rs ++ "\n")  
   ts <- forall i <- [1..3], j <- [4..6] do
      result (i,j)
   env.stdout.write (show ts ++ "\n")
   ts = forall i <- [1..3] new
               forall j <- [4..6] class
                  s := 0
                  result request
                          forall p <- [i..j] do
                            s := s + p
                          result s
   env.stdout.write (show (<-head (last ts)) ++ "\n")
   env.exit 0