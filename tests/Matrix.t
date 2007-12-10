module Matrix where

size                = primSizeArray
constArray          = primConstArray
listArray           = primListArray

record Matrix a =
  setElem :: Int -> Int -> a -> Action
  getElem :: Int -> Int -> Request a
  setRow  :: Int -> Array a -> Action
  getRow  :: Int -> Request (Array a)

  
a = constArray 10 (constArray 10 0)

l = listArray [listArray [1,2,3], listArray [4,5,6]]

b = a!3!4

c = a!5

-- x!2 = 3    Illegal; captured in Rename

m a = template

       x := a 
       
       setElem i j v = action
        x!i!j := v

       getElem i j = request
         return x!i!j

       setRow n a = action
         x!n := a

       getRow k = request
         return x!k 

       return Matrix {..}




   