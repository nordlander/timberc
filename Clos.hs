module Clos(clos) where

import Common
import Kindle



clos m                                  = return (clModule m)

{-

g :: (A, B, C, D, E, F) -> T

h = g (a,b,c)
...
h(d,e,f)

=>

record Clos_h   =
    enter :: (Clos_3_3, D, E, F) -> T
    a     :: A
    b     :: B
    c     :: C

h' :: (Clos_h, D, E, F) -> T
h'(cl,d,e,f) = g( cl.a, cl.b, cl.c, d, e, f )

h = { enter = g
      a     = a
      b     = b
      c     = c }
...
h.enter(h,d,e,f)

------

i = h(d,e)
...
i(f)

=>

record Clos_i =
    enter :: (Clos_i, F) -> T
    h     :: Clos_h
    d     :: D
    e     :: E

i' :: (Clos_i, F) -> T
i'(cl,f) = cl.h.enter(cl.d, cl.e, f)

i = { enter = i'
      h     = h
      d     = d
      e     = e }
...
i.enter(i,f)

-------------------------

e :: (B, C, D) -> E

f :: (A, B) -> (C, D) -> E
f(a,b) = let ... in e(b)
...
f(a',b',c',d')

=>

record Clos_f_public =
    enter :: (Clos_f_public,C,D) -> E

record Clos_f_private =
    enter :: (Clos_f_private,C,D) -> E
    b     :: B

e' :: (Clos_f_private,C,D) -> E
e'(cl,c,d) = e(cl.b, c, d)

f(a,b) = let ... in (Clos_f_public){ enter = e'
                                     b     = b }
...
x = f(a',b')
x.enter(x,c',d')

-}

data Env                                = Env { decls   :: [CDef],
                                                types   :: TEnv  }
                                        deriving (Eq, Show)

addTypes env te                         = env { types = te ++ types env }


clModule (Module m ds fs vs)            = Module m ds fs' vs'
  where fte                             = funEnv fs
        vte                             = valEnv vs
        fs'                             = clFDefs env fs
        vs'                             = clVDefs env' vs
        env                             = Env { decls = primDS ++ ds, types = primTE ++ fte ++ vte }
        env'                            = Env { decls = primDS ++ ds, types = primTE ++ funEnv fs' ++ vte }


clFDefs env []                          = []
clFDefs env ((x,f):fs)                  = (x,f') : clFDefs env fs
  where f'                              = clFun env f


clVDefs env vs                          = mapSnd (clVal env) vs

clVDef env (x,v)                        = (x, clVal env v)


clFun env (Fun te TWild b)              = Fun te t (clBody env' t b)
  where t                               = clInfer env' b
        env'                            = addTypes env te
clFun env (Fun te t b)                  = Fun te t (clBody (addTypes env te) t b)


clVal env (Val TWild e)                 = Val t e'
  where (t,e')                          = clExp env e
clVal env (Val t e)                     = Val t (clMatchExp env t e)


clBody env t0 (BDef fs b)               = BDef fs' (clBody env' t0 b)
  where fs'                             = clFDefs env' fs
        env'                            = addTypes env (funEnv fs)
clBody env t0 (BLet v b)                = BLet v' (clBody (addTypes env (valEnv [v'])) t0 b)
  where v'                              = clVDef env v
clBody env t0 (BCase _ e alts d)        = BCase (TId c) (clMatchExp env (TId c) e) alts' d'
  where c                               = findUnionByTag (decls env) (conOfAlt (head alts))
        alts'                           = map (clAlt env t0) alts
        d'                              = clBody env t0 d
clBody env t0 (BCase' e alts d)         = BCase' (clMatchExp env (typeOfLit l) e) alts' d'
  where l                               = litOfAlt (head alts)
        alts'                           = map (clAlt' env t0) alts
        d'                              = clBody env t0 d
clBody env t0 (BSeq b1 b2)              = BSeq (clBody env t0 b1) (clBody env t0 b2)
clBody env t0 (BBreak)                  = BBreak
clBody env t0 (BGen v b)                = BGen v' (clBody (addTypes env (valEnv [v'])) t0 b)
  where v'                              = clVDef env v
clBody env t0 (BAss x l e b)            = BAss x l (clMatchExp env (lookup' te l) e) (clBody env t0 b)
  where Class te                        = findClassByName (decls env) c
        TId c                           = lookup' (types env) x
clBody env t0 (BExp e)                  = BExp (clMatchExp env t0 e)


clAlt env t0 (Alt c x b)                = Alt c x (clBody (addTypes env [(x,TId c)]) t0 b)


clAlt' env t0 (Alt' l b)                = Alt' l (clBody env t0 b)


clInfer env (BDef fs b)                 = clInfer (addTypes env (funEnv fs)) b
clInfer env (BLet v b)                  = clInfer (addTypes env (valEnv [v])) b
clInfer env (BCase t e alts d)          = meet (clInfer env d : map f alts)
  where f (Alt c x b)                   = clInfer (addTypes env [(x,TId c)]) b
clInfer env (BCase' e alts d)           = meet (clInfer env d : map f alts)
  where f (Alt' l b)                    = clInfer env b
clInfer env (BSeq b1 b2)                = meet [clInfer env b1, clInfer env b2]
clInfer env (BBreak)                    = TWild
clInfer env (BGen v b)                  = clInfer (addTypes env (valEnv [v])) b
clInfer env (BAss x l e b)              = clInfer env b
clInfer env (BExp e)                    = t
  where (t,e')                          = clExp env e


meet []                                 = TPoly
meet (TPoly:ts)                         = meet ts
meet (TWild:ts)                         = meet ts
meet (t:ts)                             = t

{-
clInferExp env (ELit l)                 = typeOfLit l
clInferExp env (EVar x)                 = lookup' (types env) x
clInferExp env (ENew c _)               = TId c
clInferExp env (ESel _ l)               = lookup' te l
  where (c,Class te)                    = findClassByLabel (decls env) l
clInferExp env (ECall e es)             = skip (length es) (clInferExp env e)
clInferExp env (EEnter t e es)          = skip (length es) t
clInferExp env (ECast t t' e)           = t'
clInferExp env (EClos te t e vs)        = TClos (rng te) t
-}


clMatchExp env t0 e                     = match t t0 e'
  where (t,e')                          = clExp env e


clExp env e@(ELit l)                    = (typeOfLit l, e)
clExp env e@(EVar x)                    = (lookup' (types env) x, e)
clExp env (ENew c vs)                   = (TId c, ENew c (clVDefs env vs))
clExp env (ESel e l)                    = (lookup' ve l, ESel (match t (TId c) e') l)
  where (t,e')                          = clExp env e
        (c,Class ve)                    = findClassByLabel (decls env) l
clExp env (ECall e es)                  = matchApp t e' es' ts'
  where (t,e')                          = clExp env e
        (ts',es')                       = clExpList env es


clExp env (EEnter t e es)               = matchApp t e es ts'
  where e'                              = clMatchExp env t e
        (ts',es')                       = clExpList env es
clExp env (ECast from to e)             = (to, e')
  where (_,e')                          = clExp env e
clExp env (EClose te t e vs)            = (TClos (rng te) t, EClose te t e' vs')
  where e'                              = clMatchExp env' t e 
        vs'                             = clVDefs env vs
        env'                            = addTypes env (te ++ valEnv vs)


clExpList env es                        = unzip (map (clExp env) es)

closeExp ts t con e es                  = (TClos ts t, EClose (xs `zip` ts) t (con e (es ++ map EVar xs)) [])
  where xs                              = take (length ts) (abcSupply (evars e ++ evars es))


-- matchApp <funtype> <fun> <args> <argtypes>
matchApp _ (EClose te t e vs) es ts
  | len_expect == len_actual && okArgs  = (t, e')
  | len_expect < len_actual  && okArgs  = matchApp t e' es2 ts2
  where len_expect                      = length te
        len_actual                      = length ts
        (ts1,ts2)                       = splitAt len_expect ts
        (es1,es2)                       = splitAt len_expect es
        e'                              = subst (dom te `zip` es1) e
        okArgs                          = rng te == ts1
matchApp (TFun ts t) e es ts'           = matchApp' ts t ECall e es ts'
matchApp t0@(TClos ts t) e es ts'       = matchApp' ts t (EEnter t0) e es ts'
matchApp TPoly e es ts'                 = (TPoly, EEnter t (ECast TPoly t e) es)
  where t                               = TClos ts' TPoly
-- Note: the TPoly case is an incorrect approximation!  We should really *instantiate* polymorphic 
-- types and propagate our assumptions about type variables to other parts of the program as well.  
-- Eventually...
matchApp _ _ _ _                        = error "Internal: Clos.matchApp"


-- matchApp' <expected-argtypes> <return-type> <buildcon> <fun> <args> <actual-argtypes>
matchApp' expect ret con e es actual
  | len_expect == len_actual            = (ret, con e (matchList actual es expect))
  | len_expect < len_actual             = matchApp ret (con e (matchList actual1 es1 expect)) es2 actual2
  | len_expect > len_actual             = closeExp expect2 ret con e (matchList actual es expect1)
  where len_expect                      = length expect
        len_actual                      = length actual
        (es1,es2)                       = splitAt len_expect es
        (actual1,actual2)               = splitAt len_expect actual
        (expect1,expect2)               = splitAt len_actual expect


matchList actual es expect              = zipWith3 match actual expect es

-- match <actual> <expect> <expr>
match t t' e
  | t == t'                             = e
match t (TFun ts' t') e                 = error ("Internal: Clos.match " ++ show e ++ "  " ++ show t)
match t (TClos ts' t') e                = EClose (xs`zip`ts') t' (match t'' t' e') []
  where (t'',e')                        = matchApp t e (map EVar xs) ts'
        xs                              = take (length ts') (abcSupply (evars e))
match t t' e                            = ECast t t' e
-- Note: the equation above is also an incorrect approximation!  We should really *instantiate* 
-- polymorphic types and propagate our assumptions about type variables to other parts of the 
-- program as well.  Furthermore, we should perhaps allow arity matching to distribute over
-- datat structure (TId c ts), or possibly disallow mismatches in such cases.  Eventually...


{-

   Expects less than it gets...

   f :: (X',Y',Z') -> T'
   f (x,y,z) = x+y+z

   g :: ((X,Y)->((Z)->T) -> ((Z)->T)
   g (h) = h(1,2)

                  (X,Y) -> ((Z) -> T')
   g(f)  ==>   g (\(x,y) -> (\(z) -> f(x,y,z)))



   Gets less than it expects...

   f :: (Int,Int) -> ((Int) -> Int)
   f (x,y) = \(z) -> x+y+z

   g :: ((Int,Int,Int)->Int) -> Int
   g (h) = h(1,2,3)

                 (Int,Int,Int) -> Int
   g(f)  ==>   g(\(x,y,z) -> f(x,y)(z))

-}