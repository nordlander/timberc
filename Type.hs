module Type(typecheck) where
      
import Common
import Core
import Env
import Depend
import List(unzip4, zipWith3)
import Monad
import Kind
import Decls
import Reduce
import Termred
import PP


--typecheck                       :: Module -> M s Module
typecheck e2 m                     = tiModule e2 m


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


tiModule (ds',te,is') (Module v ns ds is bs) = 
                                  do let env1 = impDecls env0 ds'
                                     (env2,ds2,bs2) <- typeDecls env1 ds
                                     (env3,bs3) <- instancePreds env2 (tsigsOf isTot)
                                     (ss0,pe0,subInsts) <- tiBinds env3 (concatBinds [bs2, subInsts,subInsts'])
                                     -- Here it should be checked that the equation in subInsts follow the
                                     -- restricted rules for coercions, and that the equalities collected 
                                     -- in env3 are actually met by the equations in is, bs1 and bs2
                                     let is0  = concatBinds [bs3, subInsts, subInsts']
                                         env4 = addInsts (eqnsOf is0) env3
                                     (ss1,pe1,bs) <- tiBindsList (addTEnv0 (tsigsOf isTot) env4) (groupBinds bs)
                                     (ss2,pe2,classInsts) <- tiBinds (addTEnv0 (tsigsOf bs) env4) classInsts
                                     assert (null (pe0++pe1++pe2)) "Internal: top-level type inference"
                                     s <- unify env3 (ss0++ss1++ss2)
                                     let isRes = filterInsts v (is0 `catBinds` subst s classInsts)
                                         dsRes = filterDecls v ds2
                                         bsRes = subst s bs
                                     return (Module v ns dsRes isRes bsRes)
  where env0                    = addTEnv0 te (initEnv {modName = Just (str v)})
        isTot                   = is' `catBinds` is
        (subInsts,classInsts)   = splitInsts is
        (subInsts',classInsts') = splitInsts is'

tiBindsList env []              = return ([], [], nullBinds)
tiBindsList env (bs:bss)        = do (ss1, pe1, bs') <- tiBinds env bs
                                     (ss2, pe2, bss') <- tiBindsList (addTEnv (tsigsOf bs') env) bss
                                     return (ss1++ss2, pe1++pe2, bs' `catBinds` bss')

                                     
tiBinds env (Binds rec te eqs)  = do -- tr ("TYPE-CHECKING " ++ render (vpr te) ++ ",  rec: " ++ show rec)
                                     -- tr ("tevars: " ++ show (tevars env))
                                     (s,pe,es1)   <- tiRhs0 env' explWits ts es
                                     -- tr ("RESULT: " ++ render (vpr es1))
                                     (s',qe,f)    <- fullreduce (target te env) s pe
                                     -- tr ("PREDICATES OBTAINED: " ++ show qe)
                                     let env1      = subst s' env
                                         (qe1,qe2) = partition (isFixed env1) qe
                                         (vs,qs)   = unzip qe2
                                         es2       = map f es1
                                         es3       = if rec then map (subst sat) es2 else es2
                                         sat       = let es = map eVar vs in map (\x -> (x, \t -> eAp (h x t) es)) xs
                                         h x (Just t) = eVarT x qs t
                                         h x _        = eVar x
                                         (es',ts') = unzip (zipWith (qual qe2) es3 (subst s' ts))
                                     -- tr ("Witnesses returned: " ++ show qe1 ++ "   |   " ++ show qe2)
                                     ts'' <- mapM (gen (tevars env1 ++ tvars qe1)) ts'
                                     -- tr ("DONE " ++ render (vpr (xs `zip` ts'')))
                                     return (mkEqns s', qe1, Binds rec (xs `zip` ts'') (xs `zip` es'))
  where ts                      = map (lookup' te) xs
        (xs,es)                 = unzip eqs
        explWits                = map (explicit . annot) xs
        env'                    = if rec then addTEnv te env else env




tiRhs0 env explWits ts es       = do (ss,pes,es') <- fmap unzip3 (mapM (tiExpT' env) (zip3 explWits ts es))
                                     return (concat ss, concat pes, es')

tiRhs env ts es                 = tiRhs0 env (repeat False) ts es


tiExpT env t e                  = tiExpT' env (False, t, e)


tiExpT' env (False, Scheme t0 [] [], e)
                                = do (ss,pe,t,e)  <- tiExp env e
                                     c            <- newName coercionSym
                                     return (ss, (c, Scheme (F [scheme' t] t0) [] []) : pe, EAp (eVar c) [e])
tiExpT' env (False, Scheme t0 ps [], e) 
                                = do (ss,qe,t,e)  <- tiExp env e
                                     c            <- newName coercionSym
                                     pe0          <- newEnv assumptionSym ps
                                     let ws        = map eVar (dom pe0)
                                         e1        = eLam pe0 (EAp (eAp (eVar c) ws) [e])
                                     return (ss, (c, Scheme (F [scheme' t] t0) ps []) : qe, e1)
tiExpT' env (explWit, Scheme t0 ps ke, e)
                                = do (ss,qe,t,e)  <- tiExp env e
                                     -- tr ("INFERRED: " ++ show t ++ "   \\\\    " ++ show qe)
                                     (s,qe,f)     <- normalize (target t env) ss qe
                                     c            <- newName coercionSym
                                     pe0          <- newEnv assumptionSym ps
                                     let env1      = subst s env
                                         (qe1,qe2) = partition (isFixed env1) (subst s qe)
                                         (e',t')   = qual qe2 (f e) (scheme' (subst s t))
                                         ws        = map eVar (dom pe0)
                                         (ws',ps') = if explWit then (ws, ps) else ([], [])
                                         e1        = eLam pe0 (eAp (EAp (eAp (eVar c) ws) [e']) ws')
                                     sc           <- gen (tevars env1 ++ tvars qe1) t'
                                     return (mkEqns s, (c, Scheme (F [sc] (tFun ps' t0)) ps ke) : qe1, e1)


mkEqns s                        = mapFst TVar s

isFixed env (w,p)               = isDummy w || all (`elem` tvs) (tvars p)
  where tvs                     = tevars env


tiAp env s pe (F scs rho) e es
  | len_scs >= len_es           = do (s',pe',es') <- tiRhs env scs1 es
                                     te <- newEnv paramSym scs2
                                     return (s++s', pe++pe', tFun scs2 rho, eLam te (EAp e (es'++map eVar (dom te))))
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
                                     return (s++concat ss, (c,p):pe++concat pes, R t, EAp (EAp (eVar c) [e]) es)

tiExp env (ELit l)              = return ([], [], R (litType l), ELit l)
tiExp env (EVar x _)            = do (pe,t,e) <- instantiate (findExplType env x) (EVar (annotExplicit x))
                                     return ([], pe, t, e)
tiExp env (ECon k _)            = do (pe,t,e) <- instantiate (findExplType env k) (ECon (annotExplicit k))
                                     return ([], pe, t, e)
tiExp env (ESel e l _)          = do (F [t0] t, ps) <- inst (findType env l)
                                     (s,pe,e) <- tiExpT env t0 e
                                     let r = if explicit (annot l) then (tFun ps t,[]) else (t,ps)
                                     (pe',t',e') <- saturate r (ESel e (annotExplicit l))
                                     return (s, pe++pe', t', e')
tiExp env (ELam te e)           = do (s,pe,t,e) <- tiExp (addTEnv te env) e
                                     return (s, pe, F (rng te) t, ELam te e)
tiExp env (EAp e es)            = do (s,pe,t,e) <- tiExp env e
                                     tiAp env s pe t e es
tiExp env (ELet bs e)           = do (s,pe,bs) <- tiBinds env bs
                                     (s',pe',t,e) <- tiExp (addTEnv (tsigsOf bs) env) e
                                     return (s++s',pe++pe', t, ELet bs e)
tiExp env (ERec c eqs)          = do alphas <- mapM newTVar (kArgs (findKind env c))
                                     (t,ts,sels')   <- tiLhs env (foldl TAp (TId c) alphas) tiX sels
                                     (s,pe,es') <- tiRhs env ts es
                                     -- tr ("RECORD " ++ render (pr t))
                                     -- tr (render (vpr (sels `zip` ts)))
                                     -- tr ("    pe :\n" ++ render (vpr pe))
                                     e <- mkRec env c (map flatSels sels' `zip` es')
                                     return (s, pe, R t, e)
  where (sels,es)               = unzip eqs
        tiX env x l             = tiExp env (eSel (eVar x) l)
tiExp env (ECase e alts d)      = do alpha <- newTVar Star
                                     (t,ts,pats') <- tiLhs env alpha tiX pats
                                     (s,pe,es')   <- tiRhs env ts es
                                     let TFun [t0] t1 = t
                                     (s1,pe1,e')  <- tiExpT env (scheme t0) e
                                     (s2,pe2,d')  <- tiExpT env (scheme t1) d
                                     e <- mkCase env e' (map flatCons pats' `zip` es') d'
                                     return (s++s1++s2, pe++pe1++pe2, R t1, e)
  where (pats,es)               = unzip alts
        tiX env x (PLit l)      = tiExp env (EAp (eVar x) [ELit l])
        tiX env x (PCon k)      = do (t,_) <- inst (findType env k)
                                     te <- newEnv paramSym (fArgs t)
                                     tiExp env (eLam te (e (dom te)))
          where e vs            = EAp (eVar x) [eAp (eCon k) (map eVar vs)]
                fArgs (F ts t)  = ts
                fArgs t         = []
tiExp env (EReq e e')           = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     (s,pe,e) <- tiExpT env (scheme (tRef alpha)) e
                                     (s',pe',e') <- tiExpT env (scheme (tCmd alpha beta)) e'
                                     return (s++s', pe++pe', R (tRequest beta), EReq e e')
tiExp env (EAct e e')           = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     (s,pe,e) <- tiExpT env (scheme (tRef alpha)) e
                                     (s',pe',e') <- tiExpT env (scheme (tCmd alpha beta)) e'
                                     return (s++s', pe++pe', R tAction, EAct e e')
tiExp env (EDo x tx c)
  | isTVar tx                   = do (s,pe,t,c) <- tiCmd (setSelf x tx env) c
                                     let s' = case stateT env of Nothing -> []; Just t' -> [(t',tx)]
                                     return (s'++s, pe, R (tCmd tx t), EDo x tx c)
  | otherwise                   = error "Explicitly typed do expressions not yet implemented"
tiExp env (ETempl x tx te c)
  | isTVar tx                   = do n <- newName stateSym
                                     let env' = setSelf x (TId n) (addTEnv te (addKEnv0 [(n,Star)] env))
                                     (s,pe,t,c) <- tiCmd env' c
                                     return ((TId n, tx):s, pe, R (tTemplate t), ETempl x (TId n) te c)
  | otherwise                   = error "Explicitly typed template expressions not yet implemented"


tiCmd env (CRet e)              = do alpha <- newTVar Star
                                     (s,pe,e) <- tiExpT env (scheme alpha) e
                                     return (s, pe, alpha, CRet e)
tiCmd env (CExp e)              = do alpha <- newTVar Star
                                     (s,pe,e) <- tiExpT env (scheme (tCmd (fromJust (stateT env)) alpha)) e
                                     return (s, pe, alpha, CExp e)
tiCmd env (CGen x tx e c)       = do (s,pe,e) <- tiExpT env (scheme (tCmd (fromJust (stateT env)) tx)) e
                                     (s',pe',t,c) <- tiCmd (addTEnv [(x,scheme tx)] env) c
                                     return (s++s', pe++pe', t, CGen x tx e c)
tiCmd env (CAss x e c)          = do (s,pe,e) <- tiExpT env (findType env x) e
                                     (s',pe',t,c) <- tiCmd env c
                                     return (s++s', pe++pe', t, CAss x e c)
tiCmd env (CLet bs c)           = do (s,pe,bs) <- tiBinds env bs
                                     (s',pe',t,c) <- tiCmd (addTEnv (tsigsOf bs) env) c
                                     return (s++s', pe++pe', t, CLet bs c)


-- Compute a list of
tiLhs env alpha tiX xs          = do x <- newName tempSym
                                     let env' = addTEnv [(x,scheme alpha)] env
                                     (_,pes,ts,es) <- fmap unzip4 (mapM (tiX env' x) xs)    -- ignore returned substs (must be null)
                                     (s,_,f) <- resolve (target alpha env) (concat pes)     -- ignore returned qe (filter pes instead)
                                     let ts1 = map scheme' (subst s ts)
                                         es1 = map f es
                                         pes1 = subst s (map (filter (not . isCoercion . fst)) pes) -- preserve non-coercions for each alt
                                         (es2,ts2) = unzip (zipWith3 qual pes1 es1 ts1)
                                     ts3 <- mapM (gen (tevars (subst s env'))) ts2
                                     return (subst s alpha, ts3, map (redTerm (insts env)) es2)


-- Build record fields -----------------------------------------------------------------------------------------------------------
-- Note: ancestors reachable through different paths not yet handled

-- Extract a list of selectors from the lhs term computed by tiLhs
-- x.l   ===>   \ws -> (w x).l ws    ===>   \ws -> ((x.l1)...ln).l ws    ==>    l1, ..., ln, l
flatSels (ELam _ e)             = flatSels e
flatSels (EAp e _)              = flatSels e
flatSels e                      = flat e []
  where flat (ESel e l _) sels  = flat e (l:sels) 
        flat _ sels             = sels

-- Construct record field definitions on basis of the original rhs terms and the flattened list of selectors computed by tiLhs
mkRec env c eqs                 = liftM (ERec c) (mapM mkEqn ls)
  where ls                      = nub (map (head . fst) eqs)
        mkEqn l                 = case [ (ls,e) | (l':ls,e) <- eqs, l==l' ] of
                                    ([],e):_ -> return (l, e)
                                    eqs_l    -> do e <- mkRec env (tId (tHead t)) eqs_l
                                                   return (l, e)
                                      where Scheme (F _ (R t)) _ _ = findType env l


-- Build case alternatives -------------------------------------------------------------------------------------------------------
-- Note: ancestors reachable through different paths not yet handled

-- Extract a list of constructors from the lhs term computed by tiLhs
-- \xs -> x (k xs)  ===>  \ws -> \xs -> x (w (k ws xs))   ===>  \ws -> \xs -> x (k1 (... kn (k ws xs)))  ==>  k1, ... , kn, k
flatCons (ELam _ e)             = flatCons e
flatCons (EAp (EVar x _) [e])   = flatCons e
flatCons e                      = flat e
  where flat (EAp e es)         = flat e ++ flat (head es)      -- if e is a coercion, es will all be variables except for the head
        flat (ECon k t)         = [(PCon k, t)]
        flat (ELit l)           = [(PLit l, Nothing)]
        flat _                  = []

-- Construct case alternatives on basis of the original rhs terms and the flattened list of constructors computed by tiLhs
mkCase env e alts d             = liftM (\a -> ECase e a d) (mapM mkAlt ps)
  where ps                      = nub (map (head . fst) alts)
        mkAlt p                 = case [ (ps,e) | (p':ps,e) <- alts, p==p' ] of
                                    ([],e):_ -> return (fst p, e)
                                    alts_p   -> do x <- newName tempSym
                                                   e' <- mkCase env (eVar x) alts_p (eVar (prim Fail))
                                                   return (fst p, ELam [(x,t)] e')
                                      where F [t] _ = fromJust (snd p)




-- data Pack m a = Pack (m a) \\ Eq a
-- data Exists m > Pack m a \\ a
--               = Test (m Int)

-- Translation 1:
-- data Pack m a = Pack (m a) \\ Eq a
-- data Exists m = Exists_from_Pack (Pack m a) \\ a
--               | Test (m Int)

-- Translation 2:
-- data Pack m a = Pack (Eq a) (m a)
-- data Exists m = Exists_from_Pack (Pack m a) \\ a
--               | Test (m Int)

-- Constructor types:
-- Pack             :: Eq a -> m a -> Pack m a \\ a, m
-- Exists_from_Pack :: Pack (m a) -> Exists m \\ a, m
-- Test             :: m Int -> Exists m \\ m

-- case e0 of
--   Pack -> (\w r -> e1)
--   Test -> (\q -> e2

-- case e0 of
--   Exixts_from_Pack -> (\x -> case x of
--                                Pack -> (\w r -> e1)
--                                _    -> fail
--   Test             -> (\q -> e2)


-- Formulation without subtyping: ------------------------------
--
-- data Exists m = Pack (m a) \\ Eq a, a
--               | Test (m Int)

-- Translation:
-- data Exists m = Pack (Eq a) (m a) \\ a
--               | Test (m Int)

-- Constructor types:
-- Pack  :: Eq a -> m a -> Exists m \\ a, m
-- Test  :: m Int -> Exists m \\ m

-- case e0 of
--   Pack -> (\(w :: Eq _) (r :: _ _) -> e1)
--   Test -> (\(q :: _ Int) -> e2)





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
