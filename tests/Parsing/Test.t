module Test where

import ParsingCombinators
import POSIX

p2 = parseP (return (F$ \a->F$ \b->"42") $** token 'a' $** token 'b') "ab"
p2a = parseP (return (\a -> \b -> 42) $* token 'a' $* token 'b') "ab"

p3 = parseP (return (const 42) $* token 'a'
          $+ return (const 43) $* token 'b') "cba"

p4 = parseP (return 0 $+ return (const 1) $* token 'a') "ba"

p5' :: P [Int]
p5' = return (F$ \a -> F$ \b -> 424242:b) $** token 'a' $** p5'
  $+ return []

p5 = parseP p5' "aa"

pMany = parseP (many (token 'a')) "aaa"

data T = Leaf Token | Node T T

implicit showT :: Show T = struct
  show (Leaf t)    = "."
  show (Node t1 t2) = "("++show t1++","++show t2++")"

pT' :: P T
pT' = return (F$ \_ -> F$ \a -> F$ \_ -> F$ \b -> F$ \_ -> Node a b) $** 
      token '(' $** 
      pT'       $** 
      token ',' $** 
      pT' $** 
      token ')'
   $+ return (F$ Leaf) $** token '.'

--pT = parseP pT' "((" -- "(.(.))"

data Exp = EVar Var
         | EOp Var Op Exp 

data Var = Var Token
data Op = Op Token

implicit showExp :: Show Exp = struct
  show (EVar v) = show v
  show (EOp v o e) = "("++show v++show o++show e++")"

implicit showVar :: Show Var = struct
  show (Var v) = [v]

implicit showOp :: Show Op = struct
  show (Op o) = [o]

eVar = F$ \x -> EVar x
eOp  = F$ \x -> F$ \y -> F$ \z -> EOp x y z

var  = F$ \x -> Var x
op   = F$ \x -> Op x

---
pExp = return eOp  $** pVar $** pOp $** pExp
    $+ return eVar $** pVar
    $+                 parens pExp

pVar = return var  $** accept (\x -> x >= 'a' && x <= 'z')
pOp  = return op   $** accept (\x -> x `elem` "+-*/")
---
pExp' = parseP pExp "a"
             
root env =
    class
       start = action
                 env.stdout.write $ show $ pExp'
                 env.exit 0
       io = action
               result ()
       result Prog {..}

{-
returnLet = return Let :: m (B -> E -> Int)

coerce1 :: (E -> Int) -> (E -> {Int})
coerce0 :: (B -> E -> Int) -> (B -> (E -> Int))
coerce2 :: (B -> (E -> Int)) -> (B -> (E -> Int))

expP :: P E
expP =  coerce1 $^ (coerce2 $^ return (coerce0 Let) $* declP) $* expP

expP =  return (\a -> \b -> Let a b) $* declP $* expP
-}


{-
pExp = pOp $** pExp

a $** pExp = f pExp.emptyParse


pExp = pOp $** pExp

a $** pExp = f z

z = pExp.emptyParse
-}