module Type2 where
    
import Common
import Core
import Env
import Decls

typecheck2 e2 m                 = t2Module e2 m

t2Module (xs',ds',is',bs') (Module v ns xs ds is bs)
                                = do is <- t2Binds0 env2 is
                                     bs <- t2Binds0 env2 bs
                                     return (Module v ns xs ds is bs)
  where env2                    = addTEnv0 te2 (addKEnv0 ke2 env1)
        te2                     = tsigsOf is ++ tsigsOf bs ++ tenvSelsCons ds
        ke2                     = ksigsOf ds
        env1                    = addTEnv0 te1 (addKEnv0 ke1 env0)
        te1                     = tsigsOf is' ++ tsigsOf bs' ++ tenvSelsCons ds'
        ke1                     = ksigsOf ds'
        env0                    = addTEnv0 primPredEnv (initEnv { modName = Just (str v) })
        

t2Binds0 env (Binds r te eqs)   = do eqs <- mapM t2Eq eqs
                                     return (Binds r te eqs)
  where t2Eq (x,e)              = do s <- t2ExpT env (findType env x) e
                                     return (x, subst s e)


t2Binds env (Binds r te eqs)    = do s <- t2ExpTs env1 scs es
                                     scs <- mapM (t2Gen (subst s env)) (subst s scs)    -- Will we really need to generalize in this pass?
                                     return (s {-, Binds r (xs `zip` scs) eqs-})        -- If so, here's the place to do it
  where env1                    = if r then addTEnv te env else env
        (xs,es)                 = unzip eqs
        scs                     = rng te


-- Note: In order to end up with the proper type of e captured even in the type annotations inside e, we do not generalize
-- inferred type rh below and call mgi just to have the scheme instantiated with fresh tvars once sc has been skolemized.
-- Instead we let rh keep its unique tvars and skolemize sc right away.  Because we know our input program is type correct
-- with unique tyvars in every given type scheme, skolemization can be as simple as just dropping the scheme quantifiers.
-- However, if we really wanted to do all checks anew, we would have to explicitly treat the generalizable tvars of rh as
-- harmless w.r.t skolemization.
-- Note also that this short-cut couldn't be taken in Type, since there we want to save the constraint rh < sc for 
-- reduction at a later time where we'll have no information on which rh tvars that are generalizable and thus harmless.
t2ExpT env sc e                 = do (s1,rh) <- t2Exp env e
                                     s2 <- mgi' rh (quickSkolem sc)
                                     return (mergeSubsts [s1,s2])
                                     

t2ExpTs env [] []               = return nullSubst
t2ExpTs env (sc:scs) (e:es)     = do s1 <- t2ExpT env sc e
                                     s2 <- t2ExpTs env scs es
                                     return (mergeSubsts [s1,s2])


t2Exps env []                   = return (nullSubst, [])
t2Exps env (e:es)               = do (s1,t) <- t2Exp env e
                                     (s2,ts) <- t2Exps env es
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s (t:ts))
                                      

t2Exp env (ELit l)              = return (nullSubst, R (litType l))
t2Exp env (EVar x)              = do rh <- t2Inst (findType env x)
                                     return (nullSubst, rh)
t2Exp env (ECon k)              = do rh <- t2Inst (findType env k)
                                     return (nullSubst, rh)
t2Exp env (ESel e l)            = do F [t] rh <- t2Inst (flipSel (findType env l))
                                     s <- t2ExpT env t e
                                     return (s, subst s rh)
t2Exp env (ELam te e)           = do (s,rh) <- t2Exp (addTEnv te env) e
                                     return (s, F (subst s (rng te)) rh)
t2Exp env (EAp e es)            = do (s,rh) <- t2Exp env e
                                     t2Ap env s rh es
t2Exp env (ELet bs e)           = do s1 <- t2Binds env bs
                                     t2Exp (addTEnv (subst s1 (tsigsOf bs)) env) e
t2Exp env (ERec c eqs)          = do alphas <- mapM newTVar (kArgs (findKind env c))
                                     (t,scs) <- t2Lhs env (foldl TAp (TId c) alphas) t2Sel ls
                                     s <- t2ExpTs env scs es
                                     return (s, R (subst s t))
  where (ls,es)                 = unzip eqs
        t2Sel env x l           = t2Exp env (ESel (EVar x) l)
t2Exp env (ECase e alts d)      = do alpha <- newTVar Star
                                     (TFun [t0] t1,scs) <- t2Lhs env alpha t2Pat ps
                                     s0 <- t2ExpT env (scheme t0) e
                                     s1 <- t2ExpT env (scheme t1) d
                                     s2 <- t2ExpTs env scs es
                                     let s = mergeSubsts [s0,s1,s2]
                                     return (s, R (subst s t1))
  where (ps,es)                 = unzip alts
        t2Pat env x (PLit l)    = t2Exp env (EAp (EVar x) [ELit l])
        t2Pat env x (PCon k)    = do rh <- t2Inst (findType env k)
                                     te <- newEnv paramSym (funArgs rh)
                                     t2Exp env (eLam te (EAp (EVar x) [eAp (ECon k) (map EVar (dom te))]))
t2Exp env (EReq e1 e2)          = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     s1 <- t2ExpT env (scheme (tRef alpha)) e1
                                     s2 <- t2ExpT env (scheme (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R (tRequest (subst s beta)))
t2Exp env (EAct e1 e2)          = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     s1 <- t2ExpT env (scheme (tRef alpha)) e1
                                     s2 <- t2ExpT env (scheme (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R tAction)
t2Exp env (EDo x tx c)          = do (s1,t) <- t2Cmd (setSelf x tx env) c
                                     let s2 = case stateT env of Nothing -> nullSubst; Just t' -> unif [(t',tx)]
                                         s = mergeSubsts [s1,s2]
                                     return (s, R (subst s (tCmd tx t)))
t2Exp env (ETempl x tx te c)    = do (s,t) <- t2Cmd (setSelf x tx (addTEnv te env)) c
                                     return (s, R (tTemplate t))

        
t2Cmd env (CRet e)              = do alpha <- newTVar Star
                                     s <- t2ExpT env (scheme alpha) e
                                     return (s, subst s alpha)
t2Cmd env (CExp e)              = do alpha <- newTVar Star
                                     s <- t2ExpT env (scheme (tCmd (fromJust (stateT env)) alpha)) e
                                     return (s, subst s alpha)
t2Cmd env (CGen x tx e c)       = do s1 <- t2ExpT env (scheme (tCmd (fromJust (stateT env)) tx)) e
                                     (s2,t) <- t2Cmd (addTEnv [(x,scheme tx)] env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t)
t2Cmd env (CAss x e c)          = do s1 <- t2ExpT env (findType env x) e
                                     (s2,t) <- t2Cmd env c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t)
t2Cmd env (CLet bs c)           = do s1 <- t2Binds env bs
                                     (s2,t) <- t2Cmd (addTEnv (tsigsOf bs) env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t)

                                     

t2Ap env s1 (F scs rh) es       = do s2 <- t2ExpTs env scs es
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s rh)
t2Ap env s1 rh es               = do (s2,rhs) <- t2Exps env es
                                     t <- newTVar Star
                                     s3 <- mgi' rh (F (map scheme' rhs) (R t))
                                     let s = mergeSubsts [s1,s2,s3]
                                     return (s, R (subst s t))


t2Lhs env alpha t2X xs          = do x <- newName tempSym
                                     let env' = addTEnv [(x,scheme alpha)] env
                                     (ss,rhs) <- fmap unzip (mapM (t2X env' x) xs)
                                     let s = mergeSubsts ss
                                     scs <- mapM (t2Gen (subst s env') . scheme') (subst s rhs)
                                     return (subst s alpha, scs)


t2Gen env (Scheme rh ps ke)     = do ids <- newNames tyvarSym (length tvs)
                                     let s = tvs `zip` map TId ids
                                     return (Scheme (subst s rh) (subst s ps) (ids `zip` map tvKind tvs ++ ke))
  where tvs                     = nub (filter (`notElem` tvs0) (tvars rh))
        tvs0                    = tevars env
    

t2Inst (Scheme rh ps ke)        = do ts <- mapM newTVar ks
                                     return (subst (vs `zip` ts) (tFun ps rh))
  where (vs,ks)                 = unzip ke



mgi (Scheme rh [] [], Scheme rh' [] [])
                                = mgi' rh rh'
mgi (sc, sc')                   = do rh <- t2Inst sc
                                     mgi' rh (quickSkolem sc')


quickSkolem (Scheme rh ps ke)   = tFun ps rh


mgi' (R t) (R u)                = return (unif [(t,u)])
mgi' (F ts t) (F us u)          = do s <- mgi' t u
                                     ss <- mapM mgi (us `zip` ts)
                                     return (mergeSubsts (s:ss))
mgi' (R (TFun ts t)) rh         = mgi' (F (map scheme ts) (R t)) rh
mgi' rh (R (TFun us u))         = mgi' rh (F (map scheme us) (R u))
mgi' (R t) (F us u)             = do (t':ts) <- mapM newTVar (replicate (length us + 1) Star)
                                     let s1 = unif [(t,TFun ts t')]
                                     s2 <- mgi' (R (subst s1 t)) (F us u)
                                     return (s2@@s1)
mgi' (F ts t) (R u)             = do (u':us) <- mapM newTVar (replicate (length ts + 1) Star)
                                     let s1 = unif [(u,TFun us u')]
                                     s2 <- mgi' (F ts t) (R (subst s1 u))
                                     return (s2@@s1)


unif []                         = nullSubst
unif ((TVar n,t):eqs)
  | t == TVar n                 = unif eqs
  | otherwise                   = let s = n +-> t; s' = unif (subst s eqs) in s' @@ s
unif ((t,TVar n):eqs)           = let s = n +-> t; s' = unif (subst s eqs) in s' @@ s
unif ((TAp t u, TAp t' u'):eqs) = unif ((t,t'):(u,u'):eqs)
unif ((TId c, TId c'):eqs)
  | c == c'                     = unif eqs
unif ((TFun ts t, TFun us u):eqs)
  | length ts == length us      = unif ((t,u) : (ts `zip` us) ++ eqs)
unif eqs                        = error ("Internal: Type2.unif " ++ show eqs)


mergeSubsts ss                  = unif (mapFst TVar (concat ss))
