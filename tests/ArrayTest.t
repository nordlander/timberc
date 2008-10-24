module ArrayTest where

--a \\ (i,v) = primUpdateArray a i v

root env = class
              a := array ['x','y','z']
              m = request
                     a!1 := 'q'
                     result size a
              result False
