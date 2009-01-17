module SilentResultUnit where

cl :: Action -> Action -> _
cl m1 m2 = class
    s := ""
    req1 x = request
              if x then
                  m1
              else
                  m2
    act1 x = action
              if x then
                  m1
              else
                  s := "1"
    req2 x = request
              if x then
                  s := "2"
    req3 x = request
              if x then
                  m1
              else
                  s := "3"
              m2
    result req1
