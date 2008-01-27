module ArrayTest where

a \\ (i,v) = primUpdateArray a i v

root env = template
              a := array ['x','y','z']
              b := uniarray 4 0.0 \\ (2,3.14)
              m = request
                     a!1 := 'q'
                     return arraysize b
              return False
