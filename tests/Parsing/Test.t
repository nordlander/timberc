module Test where

import ParsingCombinators
import POSIX
import PreludeExtra

data F a = F a
unF (F a) = a

($**) :: m (F (a -> b)) -> m a -> m b \\ Applicative m
a $** b = unF $^ a $* b

p2 :: ([Token],Maybe String)
p2 = parseP (return (F$ \a -> F$ \b -> "42") $** token 'a' $** token 'b') ""

root env =
    class
       start = action
                 env.stdout.write $ show $ p2
       io = action
               result ()
       result Prog {..}

