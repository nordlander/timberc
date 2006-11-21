module Type where
      
import Maybe
import Common
import Core
import Env
import Depend
import List(unzip4, zipWith3)
import Kind
import Decls
import Reduce
import Termred


typecheck                       :: Module -> M Module
typecheck m                     = tiModule m


{-

Example 1:
==========

-- Initially:
let f :: P => S -> T
    f = \x -> e (f 7)

-- After exp inference:
    \x -> e w (f v 7)                                ::X->T'   \\ w::W, v::V

-- After generalization:
    \w v x -> e w (f v 7)                            ::W->V->X->T'  \\

-- After mu:
let f = c (\w v x -> e w (f v 7))                  ::P->S->T   \\ c::(W->V->X->T')->(P->S->T)

-- After improvement (T' = T, X = S, V = P):
let f = c (\w v x -> e w (f v 7))                  ::P->S->T   \\ c::(W->P->S->T)->(P->S->T)

-- After reduce:
let f = (\c -> c (\w v x -> e w (f v 7))) (\i -> i w0)             ::P->S->T  \\ w0 :: W

-- After generalization:
let f = \w0 -> (\f -> ((\c -> c (\w v x -> e w (f v 7))) (\i -> i w0))) (f w0)   ::W->P->S->T  \\

-- After inlining:
let f = \w0 -> \f -> ((\c -> c (\w v x -> e w (f v 7))) (\i -> i w0)) (f w0)  ::W->P->S->T  \\
let f = \w0 -> (\c -> c (\w v x -> e w (f w0 v 7))) (\i -> i w0)              ::W->P->S->T  \\
let f = \w0 -> (\i -> i w0) (\w v x -> e w (f w0 v 7))                        ::W->P->S->T  \\
let f = \w0 -> (\w v x -> e w (f w0 v 7)) w0                                  ::W->P->S->T  \\
let f = \w0 v x -> e w0 (f w0 v 7)                                            ::W->P->S->T  \\

---------------
\i -> i w0 :: (W->P->S->T)->P->S->T                \\ w0 :: W
---------------
-}


tiModule (Module v ds is bs)    = do (env1,ds1) <- typeDecls env0 ds
                                     env2 <- classPreds env1 is
                                     (ss1,pe1,bs2) <- tiBindsList (addTEnv0 (tsigsOf is) env2) (groupBinds bs)
                                     (ss2,pe2,is1) <- tiBinds (addTEnv0 (tsigsOf bs2) env2) is
                                     assert (null (pe1++pe2)) "Internal: top-level type inference"
                                     s <- unify env2 (ss1++ss2)
                                     return (Module v ds1 nullBinds (subst s is1 `catBinds` subst s bs2))
  where env0                    = initEnv


tiBindsList env []              = return ([], [], nullBinds)
tiBindsList env (bs:bss)        = do (ss1, pe1, bs1) <- tiBinds env bs
                                     (ss2, pe2, bs2) <- tiBindsList (addTEnv0 (tsigsOf bs1) env) bss
                                     return (ss1++ss2, pe1++pe2, catBinds bs1 bs2)


tiBinds env (Binds rec te eqs)  = do --tr ("TYPE-CHECKING " ++ showids xs)
                                     (s,pe,es1)   <- tiRhs0 env' explWits ts es
                                     --tr ("Witnesses obtained: " ++ showids (dom pe))
                                     (s',qe,f)    <- fullreduce (target te env) s pe
                                     let env1      = subst s' env
                                         ts1       = subst s' ts
                                         te1       = dom te `zip` ts1
                                         (qe0,qe1) = partition (isFixed env1) qe
                                         es2       = map f es1
                                         es2'      = if rec then map (\e -> eAp (eLam te1 e) as) es2 else es2
                                         es3       = {- map redTerm -} es2'
                                         as        = let vs = dom qe1 in map (\x -> eAp' x vs) xs
                                         (es',ts') = unzip (zipWith (qual qe1) es3 ts1)
                                     --tr ("Witnesses returned: " ++ showids (dom qe0))
                                     ts'' <- mapM (gen env1) ts'
                                     return (mkEqns s', qe0, Binds rec (xs `zip` ts'') (xs `zip` (subst s' es')))
  where ts                      = rng te
        (xs,es)                 = unzip eqs
        explWits                = map (explicit . annot) xs
        env'                    = if rec then addTEnv te env else env
    


tiRhs0 env explWits ts es       = do (ss,pes,es') <- fmap unzip3 (mapM (tiExpT env) (zip3 explWits ts es))
                                     return (concat ss, concat pes, es')

tiRhs env ts es                 = tiRhs0 env (repeat False) ts es


tiExpT env (False, Scheme t0 [] [], e)
                                = do (ss,pe,t,e)  <- tiExp env e
                                     c            <- newName coercionSym
                                     return (ss, (c, Scheme (F [scheme' t] t0) [] []) : pe, EAp (EVar c) [e])
tiExpT env (False, Scheme t0 ps [], e) 
                                = do (ss,pe,t,e)  <- tiExp env e
                                     c            <- newName coercionSym
                                     pe0          <- newEnv assumptionSym ps
                                     let e1        = ELam pe0 (EAp (EVar c) (map EVar (dom pe0) ++ [e]))
                                     return (ss, (c, Scheme (F [scheme' t] t0) ps []) : pe, e1)
tiExpT env (explWit, Scheme t0 ps ke, e)
                                = do (ss,pe,t,e)  <- tiExp env e
                                     s            <- unify env ss
                                     c            <- newName coercionSym
                                     pe0          <- newEnv assumptionSym ps
                                     let env1      = subst s env
                                         (pe1,pe2) = partition (isFixed env1) (subst s pe)
                                         (e',t')   = qual pe2 e (scheme' (subst s t))
                                         ws        = map EVar (dom pe0)
                                         (ws',ps') = if explWit then (ws, map norm ps) else ([], [])
                                         e1        = ELam pe0 (EAp (EVar c) (ws ++ [e'] ++ ws'))
                                     sc <- gen env1 t'
                                     return (mkEqns s, (c, Scheme (F (sc : ps') t0) ps ke) : pe1, e1)


mkEqns s                        = mapFst TVar s

isFixed env pred                = all (`elem` tvs) (tvars pred)
  where tvs                     = tevars env


tiAp env s pe (F scs rho) e es
  | len_scs >= len_es           = do (s',pe',es') <- tiRhs env scs1 es
                                     te <- newEnv paramSym scs2
                                     return (s++s', pe++pe', tFun scs2 rho, eLam te (EAp e (es++map EVar (dom te))))
  | otherwise                   = do (s',pe',es') <- tiRhs env scs es1
                                     tiAp env (s++s') (pe++pe') rho (EAp e es') es2
  where len_scs                 = length scs
        len_es                  = length es
        (scs1,scs2)             = splitAt len_es scs
        (es1,es2)               = splitAt len_scs es
tiAp env s pe (R (TFun ts t)) e es
                                = tiAp env s pe (F (map scheme ts) (R t)) e es
tiAp env s pe rho e es          = do t <- newTVar Star
                                     c <- newName coercionSym
                                     (ss,pes,ts,es) <- fmap unzip4 (mapM (tiExp env) es)
                                     let p = Scheme (F [scheme' rho] (F (map scheme' ts) (R t))) [] []
                                     return (s++concat ss, (c,p):pe++concat pes, R t, EAp (EAp (EVar c) [e]) es)


tiExp env (ELit l)              = return ([], [], R (litType l), ELit l)
tiExp env e@(EVar x)
  | explicit (annot x)          = do (t,ps) <- inst (findType env x)
                                     return ([], [], tFunCat ps t, e)
  | otherwise                   = do (pe,t,e) <- instantiate (findType env x) e
                                     return ([], pe, t, e)
tiExp env (ELam te e)           = do (s,pe,t,e) <- tiExp (addTEnv te env) e
                                     return (s, pe, F (rng te) t, ELam te e)
tiExp env (EAp e es)            = do (s,pe,t,e) <- tiExp env e
                                     tiAp env s pe t e es
tiExp env (ELet bs e)           = do (s,pe,bs) <- tiBindsList env (groupBinds bs)
                                     (s',pe',t,e) <- tiExp (addTEnv (tsigsOf bs) env) e
                                     return (s++s',pe++pe', t, ELet bs e)
tiExp env e@(ECon k)            = do (pe,t,e) <- instantiate (findType env k) e
                                     te       <- newEnv paramSym (fst (splitC t))
                                     return ([], pe, t, eLam te (eAp e (map EVar (dom te))))
tiExp env e@(ESel l)            = do x        <- newName tempSym
                                     (pe,t,e) <- instantiate (findType env l) (EAp e [EVar x])
                                     return ([], pe, t, ELam [(x, fst (splitS t))] e)

tiExp env (ERec eqs)            = do (t,ts,_)   <- tiLhs env tiX sels
                                     (s,pe,es') <- tiRhs env ts es
                                     return (s, pe, R t, ERec (sels `zip` es'))
  where (sels,es)               = unzip eqs
        tiX env x l             = tiExp env (EAp (ESel l) [EVar x])

tiExp env (ECase e alts d)      = do (t,ts,_)    <- tiLhs env tiX pats
                                     (s,pe,es')  <- tiRhs env ts es
                                     let (t0,t1)  = splitF t
                                     (s1,pe1,e') <- tiExpT env (False, scheme t0, e)
                                     (s2,pe2,d') <- tiExpT env (False, scheme t1, d)
                                     return (s++s1++s2, pe++pe1++pe2, R t1, ECase e' (pats `zip` es') d')
  where (pats,es)               = unzip alts
        tiX env x (PLit l)      = tiExp env (EAp (EVar x) [ELit l])
        tiX env x (PCon k)      = do (t,_) <- inst (findType env k)
                                     te <- newEnv paramSym (fst (splitC t))
                                     tiExp env (eLam te (e (dom te)))
          where e vs            = EAp (EVar x) [EAp (ECon k) (map EVar vs)]
                                     

tiLhs env tiX xs                = do alpha <- newTVar Star
                                     x <- newName tempSym
                                     let env' = addTEnv [(x,scheme alpha)] env
                                     (_,pes,ts,es) <- fmap unzip4 (mapM (tiX env' x) xs)
                                     let env'' = target alpha env'
                                     (s,_,f) <- resolve' (target alpha env') (concat pes)
                                     let ts1 = map scheme' (subst s ts)
                                         es1 = map f es
                                         pes1 = subst s (map (filter (not . isCoercion . fst)) pes)
                                         (es2,ts2) = unzip (zipWith3 qual pes1 es1 ts1)
                                     ts3 <- mapM (gen (subst s env')) ts2
                                     return (subst s alpha, ts3, es2)



litType (LInt i)                = TId (prim Int)
litType (LRat r)                = TId (prim Float)
litType (LChr c)                = TId (prim Char)
litType (LStr s)                = TAp (TId (prim LIST)) (TId (prim Char))



splitF (TFun [t] t')            = (t,t')


splitS (F [t] t')               = (t, scheme' t')


splitC (F ts t)                 = (ts, scheme' t)
splitC t                        = ([], scheme' t)



{-
   
    record A v
       a :: Eq v => v -> [v]            w : Eq v |- (.a) : A v -> v -> [v]  =  \t -> t.a w

    record B v < A v =
       b :: v                                    |- (.b) : B v -> v         =  \t -> t.b


    { a v = [v], b = 0 }

    w : Eq v, ca : xx < A v |- (.a) x  :  v -> [v]  =  (\t -> t.a w) (ca x)  

              cb : xx < B v |- (.b) x  :  v         =  (\t -> t.b)   (cb x)

    b2a/ca, id/cb, B v / xx

    w : Eq v |- (.a) x  :  v -> [v]  =  (\t -> t.a w) ((.b2a) x)   =  x.b2a.a w   =  (.a) . (.b2a) .

             |- (.b) x  :  v         =  (t -> t.b)    (id x)       =  x.b


-}

{-

record A =
    a :: Ta

record B < A =
    b :: Tb

record C < A =
    c :: Tc

record D < B,C =
    d :: Td

===>

record A =
    a :: Ta

record B =
    b2a :: A
    b   :: Tb

record C =
    c2A :: A
    c   :: Tc

record D =
    d2b :: B
    d2c :: C
    d   :: Td


D < B < A
D < C < A

{ a|b2a|d2b = Ma
  a|c2a|d2c = Ma
  b|d2b     = Mb         :: D
  c|d2c     = Mc
  d         = Md
}

{ d2b = { b2a = { a = Ma }
          b   = Mb }
  d2c = { c2a = { a = Ma }
          c   = Mc }
  d   = Md }

        A
      /   \
     B     C
      \   /
        D

{ B2A -> { D2B -> { D -> \x.Md }
           B   -> \x.Mb }
  C2A -> { D2C -> { D -> \x.Md }
           C   -> \x.Mc }
  A   -> \x.Ma }

{ B2A|D2B|D -> \x.Md
  C2A|D2C|D -> \x.Md
  B2A|B     -> \x.Mb       :: A -> t
  C2A|C     -> \x.Mc
  A         -> \x.Ma
}

A->t < B->t < D->t
A->t < C->t < D->t

data D =
    D Td

data C > D =
    C Tc

data B > D =
    B Rb

data A > B,C =
    a Ta

===>

data D =
    D Td

data C =
    D2C D
    C Tc

data B =
    D2B D
    B Tb

data A =
    B2A B
    C2A C
    A Ta

-}
