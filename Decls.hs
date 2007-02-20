module Decls where

import Common
import Core
import Env
import Reduce


-- Declaration processing ---------------------------------------------------------------------

{-

?    Check acyclic tsym deps   
?    Generate future subtype instances
?    Generate future class instances

-}


-- Extend kind environment
-- Initialize empty class witness graph
-- Extract selector and constructor type schemes
-- Construct class member type schemes and bindings
-- Replace subtyping in type declarations with explicit selectors/constructors
-- Initialize subtyping graph with reflexivity witnesses
-- Close subtyping graph under transitivity (report cyclic and ambiguity errors)
-- Return extended environment, transformed decls and added witness bindings

typeDecls env (Types ke ds)             = do (ds,pe1,eq1) <- desub env0 ds
                                             (env',pe2,eq2) <- closePreds env0 [] pe1 []
                                             let (pe3,eq3) = preferParams env' pe1 pe2 eq2
                                                 te0 = concatMap (tenvSelCon env) ds
                                             return (addTEnv0 te0 env', Types ke ds, Binds False (pe1++pe3) (eq1++eq3))
  where env0                            = addClasses cs (addKEnv ke env)
        cs                              = [ c | (c, DRec True _ _ _) <- ds ]


-- Close the top-level instance delcarations
-- Return the extended environment and the added witness bindings

instancePreds env pe                    = do (env',qe,eq) <- closePreds env [] pe []
                                             let (qe',eq') = preferParams env' pe qe eq
                                             return (env', Binds False qe' eq')


-- Computes the stand-alone type schemes associated with selectors and constructors
-- Note: these constants have no corresponding definition (i.e., no rhs)

tenvSelCon env (c,DRec _ vs _ ss)       = map (f t ke) ss
  where (t,ke)                          = mkHead env c vs
        f t ke (l, Scheme rh ps ke')    = (l, Scheme (F [scheme t] rh) ps (ke++ke'))
tenvSelCon env (c,DData vs _ cs)        = map (f t ke) cs
  where (t,ke)                          = mkHead env c vs
        f t ke (k, Constr ts ps ke')    = (k, Scheme (tFun' ts t) ps (ke++ke'))
tenvSelCon env _                        = []


mkHead env i vs                         = (tAp' i vs, vs `zip` kArgs (findKind env i))


-- Decomposition of type declarations ---------------------------------------------------------

desub env ds                            = do (ds',pes,eqs) <- fmap unzip3 (mapM desub' ds)
                                             return (ds', concat pes, concat eqs)
  where 
    desub' (i, DData vs bs cs)          = do (pe,eq,cs') <- fmap unzip3 (mapM (con (mkHead env i vs)) bs)
                                             return ((i, DData vs [] (cs'++cs)), pe, eq)
    desub' (i, DRec isC vs bs ss)       = do (pe,eq,ss') <- fmap unzip3 (mapM (sel (mkHead env i vs)) bs)
                                             return ((i, DRec isC vs [] (ss'++ss)), pe, eq)
    desub' (i, DType vs t)              = return ((i, DType vs t), [], [])
    con (t0,ke0) (Scheme (R t) [] ke)   = do w <- newName coercionSym
                                             k <- newName constrSym
                                             x <- newName paramSym
                                             let p  = (w, Scheme (R (t `sub` t0)) [] (ke0++ke))
                                                 eq = (w, ELam [(x,scheme t)] (EAp (eCon k) [eVar x]))
                                                 c  = (k, Constr [scheme t] [] ke)
                                             return (p, eq, c)
    sel (t0,ke0) (Scheme (R t) [] ke)   = do w <- newName coercionSym
                                             l <- newName labelSym
                                             x <- newName paramSym
                                             let p  = (w, Scheme (R (t0 `sub` t)) [] (ke0++ke))
                                                 eq = (w, ELam [(x,scheme t0)] (eSel (eVar x) l))
                                                 s  = (l, Scheme (R t) [] ke)
                                             return (p, eq, s)

{-

   *data Exists m = All a . Pack (All b . D m b a) (All b . C m b a -> m b a)

    Pack :: All m,a . (All b . D m b a) -> (All b . C m b a -> m b a) -> Exists m

    f1 :: All a,b . (All b . D m b a) -> D m' b a
    f1 :: All a . (All b . D m b a) -> (All b . D m' b a)
    f2 :: All a,b . (All b . D m b a) -> (All b . C m b a -> m b a) -> C m' b a -> m' b a
      f2 :: All a . (All b . D m b a) -> (All b . C m b a -> m b a) -> (All b . C m' b a -> m' b a)
    case x of
      Pack -> \d::(All b . D m b a) -> \r::(All b . C m b a -> m b a) -> Pack (f1 d) (f2 d r)


   *data Exists m = All a . (All b . D m b a) => Pack (All b . C m b a -> m b a)

    Pack :: All m,a . (All b . D m b a) => (All b . C m b a -> m b a) -> Exists m

    f1 :: All a,b . (All b . D m b a) -> D m' b a
      f1 :: All a . (All b . D m b a) -> (All b . D m' b a)
    f2 :: All a,b . (All b . D m b a) -> (All b . C m b a -> m b a) -> C m' b a -> m' b a
      f2 :: All a . (All b . D m b a) -> (All b . C m b a -> m b a) -> (All b . C m' b a -> m' b a)
    case x of
      Pack -> \d::(All b . D m b a) => \r::(All b . C m b a -> m b a) -> Pack (f1 d) (f2 d r)


    
   *data Exists m = Pack (D m b a \\ b) (C m b a -> m b a \\ b) \\ a

    Pack :: (D m b a \\ b) -> (C m b a -> m b a \\ b) -> Exists m \\ m, a

    f1 :: (D m b a \\ b) -> D m' b a \\ a, b
      f1 :: (D m b a \\ b) -> (D m' b a \\ b) \\ a
    f2 :: (D m b a \\ b) -> (C m b a -> m b a \\ b) -> C m' b a -> m' b a \\ a, b
      f2 :: (D m b a \\ b) -> (C m b a -> m b a \\ b) -> (C m' b a -> m' b a \\ b) \\ a
    case x of
      Pack -> \d::(D m b a \\ b) -> \r::(C m b a -> m b a \\ b) -> Pack (f1 d) (f2 d r)


   *data Exists m = Pack (m b a \\ b, C m b a) \\ a, (D m b a \\ b)

    Pack :: (m b a \\ b, C m b a) -> Exists m \\ m, a, (D m b a \\ b)

    f1 :: (D m b a \\ b) -> D m' b a \\ a,b
      f1 :: (D m b a \\ b) -> (D m' b a \\ b) \\ a
    f2 :: (m b a \\ b, C m b a) -> m' b a \\ a, b, C m' b a, (D m b a \\ b)
      f2 :: (m b a \\ b, C m b a) -> (m' b a \\ b, C m' b a) \\ a, (D m b a \\ b)
    case x of
      Pack -> \d::(D m b a \\ b) => \r::(m b a \\ b, C m b a) -> Pack (f1 d) (f2 d r)



   *record T a =
        x :: b -> a -> b \\ b, b < a

    x :: T a -> b -> a -> b \\ b, b < a

    f :: (b -> a -> b \\ b, b < a) -> b -> a' -> b \\ b, b < a'
      f :: (b -> a -> b \\ b, b < a) -> (b -> a' -> b \\ b, b < a')
    { x = f r.x }



   *record T a =
        x :: (b<a) -> b -> a -> b \\ b

    x :: T a -> (b<a) -> b -> a -> b \\ b

    f :: ((b<a) -> b -> a -> b \\ b) -> (b<a') -> b -> a' -> b \\ b
      f :: ((b<a) -> b -> a -> b \\ b) -> ((b<a') -> b -> a' -> b \\ b)
    { x = f r.x }
-}

