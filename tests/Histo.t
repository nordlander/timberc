module Histo where

record Histogram =
  addObs    :: Float -> Action
  getResult :: Request (Array Int)

histo cs = template

   obs := primConstArray (length cs + 1) 0

   addObs x = action

     classIndex x k [] = k
     classIndex x k (y : ys)
            | x < y = k
            | otherwise = classIndex x (k+1) ys

     k = classIndex x 0 cs

     obs!k := obs!k + 1

   getResult = request
     return obs

   return Histogram {..}

