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


classPreds env (Binds r pe eqs)         = do (env,eqcs) <- closeWits env [] (mapFst eVar pe)
                                             -- To do: check that the eqcs are respected by eqs
                                             return env


-- Extend kind environment
-- Initialize empty class witness graph
-- Extract selector and constructor type schemes
-- Construct class member type schemes and bindings
-- Replace subtyping in type declarations with explicit selectors/constructors
-- Initialize subtyping graph with reflexivity witnesses
-- Close subtyping graph under transitivity (report cyclic and ambiguity errors)
-- Return extended environment, transformed decls and class membner bindings

typeDecls env (Types ke ds)             = do env1 <- initRefl ke env0
                                             (env2,eqcs) <- closeWits env1 [] (concat axioms)
                                             -- [ What do we do with the eqcs? ]
                                             return (env2, Types ke ds1)
  where env0                            = addSelCon ds (initClasses cs (addKEnv ke env))
        (ds1,axioms)                    = unzip (map (desub env0) ds)
        cs                              = [ c | (c, DRec True _ _ _) <- ds ]


-- Computes the stand-alone type schemes associated with selectors and constructors
-- Note: these constants have no corresponding definition (i.e., no rhs)

addSelCon [] env                        = env
addSelCon ((c,DRec _ vs _ ss) : ds) env = addSelCon ds (addTEnv (map (f t ke) ss) env)
  where (t,ke)                          = mkHead env c vs
        f t ke (l, Scheme rh ps ke')    = (l, Scheme (F [scheme t] rh) ps (ke++ke'))
addSelCon ((c,DData vs _ cs) : ds) env  = addSelCon ds (addTEnv (map (f t ke) cs) env)
  where (t,ke)                          = mkHead env c vs
        f t ke (k, Constr ts ps ke')    = (k, Scheme (tFun' ts t) ps (ke++ke'))
addSelCon (_ : ds) env                  = addSelCon ds env


mkHead env i vs                         = (tAp' i vs, vs `zip` kArgs (findKind env i))


-- Decomposition of type declarations ---------------------------------------------------------


dcsym i t                               = name0 ("Coerce_" ++ show (headsym t) ++ "_" ++ show i)

rcsym i t                               = name0 ("coerce_" ++ show i ++ "_" ++ show (headsym t))


desub env (i, DData vs bs cs)           = ((i, DData vs [] (ws `zip` map constr bs ++ cs)), wits `zip` map pred bs)
  where ws                              = map (dcsym i) bs
        wits                            = map eCon ws
        constr (Scheme (R t) _ ke)      = Constr [scheme t] [] ke
        pred (Scheme (R t) _ ke)        = Scheme (R (t `sub` t0)) [] (ke0++ke)
        (t0,ke0)                        = mkHead env i vs
desub env (i, DRec isC vs bs ss)        = ((i, DRec isC vs [] (ws `zip` bs ++ ss)), wits `zip` map pred bs)
  where ws                              = map (rcsym i) bs
        wits                            = map (\w -> ELam [(name0 "x", scheme t0)] (eSel (eVar (name0 "x")) w)) ws     -- temporary....
        pred (Scheme (R t) _ ke)        = Scheme (R (t0 `sub` t)) [] (ke0++ke)
        (t0,ke0)                        = mkHead env i vs
desub env (i, DType vs t)               = ((i, DType vs t), [])




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

