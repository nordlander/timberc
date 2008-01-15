module Test where

fix f = f (fix f)

x = fix (\g n -> g n)
