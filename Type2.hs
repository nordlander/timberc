module Type2 where
    
import Common
import Core
import Env
import Decls
import PP

typecheck2 e2 m                 = t2Module e2 m

t2Module (xs',ds',ws',bs') (Module v ns xs ds ws bss)
                                = do bss <- mapM (t2Binds0 env2) bss
                                     return (Module v ns xs ds ws bss)
  where env2                    = addTEnv0 te2 (addKEnv0 ke2 env1)
        te2                     = concatMap tsigsOf bss ++ tenvSelsCons ds
        ke2                     = ksigsOf ds
        env1                    = addTEnv0 te1 (addKEnv0 ke1 env0)
        te1                     = tsigsOf bs' ++ tenvSelsCons ds'
        ke1                     = ksigsOf ds'
        env0                    = addTEnv0 primPredEnv (initEnv v)
        

t2Binds0 env (Binds r te eqs)   = do (s,eqs) <- t2Eqs eqs
                                     return (Binds r (subst s te) eqs)          -- !
  where t2Eqs []                = return (nullSubst, [])
        t2Eqs ((x,e):eqs)       = do t <- t2Inst sc             -- Note: don't skolemize, use unification variables instead,
                                     (s1,e) <- t2ExpT env t e   -- since no unification errors should appear in this pass anyway 
                                     (s2,eqs) <- t2Eqs eqs      
                                     return (mergeSubsts[restrict s1 (tvars sc),s2], (x, subst s1 e):eqs)  -- !
          where sc              = findType env x


t2Binds env (Binds r te eqs)    = do ts <- mapM t2Inst scs
                                     (s,es) <- t2ExpTs env1 ts es
                                     scs <- mapM (t2Gen (subst s env)) (subst s ts)                        -- !
                                     return (s, Binds r (xs `zip` scs) (xs `zip` es))
  where env1                    = if r then addTEnv te env else env
        (xs,es)                 = unzip eqs
        scs                     = rng te


-- The main purpose of Type2 is to assign correct inferred types not only to the binding nodes, but to all syntactic 
-- nodes in the scope of such a binding as well.  However, since Timber does not (yet?) support scoped type variables,
-- we cannot just apply substitutions containing generalized type variables (lower case TId's) to inner nodes.  Instead
-- we want to work with unification variables (TVar's) only, as these will be approximated as wildcard types (_) when the
-- resulting program is output.  Luckily this is ok, because we can rely on the program being type correct at this stage.
-- Thus any distinction between generalized variables (that must be kept untouched) and arbitrary unification variables
-- is just artificial here, and we can proceed using fresh unification variables in type environments created when we
-- step in under a polymorphic binding.

t2ExpT env t0 e                 = do (s1,t1,e) <- t2Exp env e
                                     s2 <- mgi t0 t1
                                     return (mergeSubsts [s1,s2], e)
                                     

t2ExpTs env [] []               = return (nullSubst, [])
t2ExpTs env (t:ts) (e:es)       = do (s1,e) <- t2ExpT env t e
                                     (s2,es) <- t2ExpTs env ts es       
                                     return (mergeSubsts [s1,s2], e:es)


t2Exps env []                   = return (nullSubst, [], [])
t2Exps env (e:es)               = do (s1,t,e) <- t2Exp env e
                                     (s2,ts,es) <- t2Exps env es
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s (t:ts), e:es)
                                      

t2Exp env (ELit l)              = return (nullSubst, R (litType l), ELit l)
t2Exp env (EVar x)              = do rh <- t2Inst (findType env x)
                                     return (nullSubst, rh, EVar x)
t2Exp env (ECon k)              = do rh <- t2Inst (findType env k)
                                     return (nullSubst, rh, ECon k)
t2Exp env (ESel e l)            = do F (sc:scs) rh <- t2Inst (findType env l)
                                     t <- t2Inst sc                 -- (we know sc isn't really polymorphic)
                                     (s,e) <- t2ExpT env t e
                                     return (s, subst s (tFun scs rh), ESel e l)
t2Exp env (ELam te e)           = do (s,rh,e) <- t2Exp (addTEnv te env) e
                                     return (s, F (subst s (rng te)) rh, ELam te e)
t2Exp env (EAp e es)            = do (s,rh,e) <- t2Exp env e
                                     t2Ap env s rh e es
t2Exp env (ELet bs e)           = do (s1,bs) <- t2Binds env bs
                                     (s2,rh,e) <- t2Exp (addTEnv (subst s1 (tsigsOf bs)) env) e
                                     return (mergeSubsts [s1,s2], rh, ELet bs e)
t2Exp env (ERec c eqs)          = do alphas <- mapM newTVar (kArgs (findKind env c))
                                     (t,ts) <- t2Lhs env (foldl TAp (TId c) alphas) t2Sel ls
                                     (s,es) <- t2ExpTs env ts es
                                     return (s, R (subst s t), ERec c (ls `zip` es))
  where (ls,es)                 = unzip eqs
        t2Sel env x l           = t2Exp env (ESel (EVar x) l)
t2Exp env (ECase e alts)        = do alpha <- newTVar Star
                                     (TFun [t0] t1,ts) <- t2Lhs env alpha t2Pat ps
                                     (s0,e) <- t2ExpT env (R t0) e
                                     (s1,es) <- t2ExpTs env ts es
                                     let s = mergeSubsts [s0,s1]
                                     return (s, R (subst s t1), ECase e (ps `zip` es))
  where (ps,es)                 = unzip alts
        t2Pat env x (PLit l)    = t2Exp env (EAp (EVar x) [ELit l])
        t2Pat env x (PCon k)    = do rh <- t2Inst (findType env k)
                                     te <- newEnv paramSym (funArgs rh)
                                     t2Exp env (eLam te (EAp (EVar x) [eAp (ECon k) (map EVar (dom te))]))
        t2Pat env x (PWild)     = do y <- newName tempSym
                                     t <- newTVar Star
                                     t2Exp (addTEnv [(y,scheme t)] env) (EAp (EVar x) [EVar y])
t2Exp env (EReq e1 e2)          = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     (s1,e1) <- t2ExpT env (R (tRef alpha)) e1
                                     (s2,e2) <- t2ExpT env (R (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R (tRequest (subst s beta)), EReq e1 e2)
t2Exp env (EAct e1 e2)          = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     (s1,e1) <- t2ExpT env (R (tRef alpha)) e1
                                     (s2,e2) <- t2ExpT env (R (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R tAction, EAct e1 e2)
t2Exp env (EDo x tx c)          = do (s1,t,c) <- t2Cmd (setSelf x tx env) c
                                     let s2 = case stateT env of Nothing -> nullSubst; Just t' -> unif [(t',tx)]
                                         s = mergeSubsts [s1,s2]
                                     return (s, R (subst s (tCmd tx t)), EDo x tx c)
t2Exp env (ETempl x tx te c)    = do (s,t,c) <- t2Cmd (setSelf x tx (addTEnv te env)) c
                                     return (s, R (tClass t), ETempl x tx te c)

        
t2Cmd env (CRet e)              = do alpha <- newTVar Star
                                     (s,e) <- t2ExpT env (R alpha) e
                                     return (s, subst s alpha, CRet e)
t2Cmd env (CExp e)              = do alpha <- newTVar Star
                                     (s,e) <- t2ExpT env (R (tCmd (fromJust (stateT env)) alpha)) e
                                     return (s, subst s alpha, CExp e)
t2Cmd env (CGen x tx e c)       = do (s1,e) <- t2ExpT env (R (tCmd (fromJust (stateT env)) tx)) e
                                     (s2,t,c) <- t2Cmd (addTEnv [(x,scheme tx)] env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CGen x tx e c)
t2Cmd env (CAss x e c)          = do t0 <- t2Inst (findType env x)
                                     (s1,e) <- t2ExpT env t0 e
                                     (s2,t,c) <- t2Cmd env c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CAss x e c)
t2Cmd env (CLet bs c)           = do (s1,bs) <- t2Binds env bs
                                     (s2,t,c) <- t2Cmd (addTEnv (tsigsOf bs) env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CLet bs c)

                                     

t2Ap env s1 (F scs rh) e es     = do ts <- mapM t2Inst scs
                                     (s2,es) <- t2ExpTs env ts es
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s rh, EAp e es)
t2Ap env s1 rh e es             = do (s2,rhs,es) <- t2Exps env es
                                     t <- newTVar Star
                                     s3 <- mgi rh (F (map scheme' rhs) (R t))
                                     let s = mergeSubsts [s1,s2,s3]
                                     return (s, R (subst s t), EAp e es)


t2Lhs env alpha t2X xs          = do x <- newName tempSym
                                     let env' = addTEnv [(x,scheme alpha)] env
                                     (ss,rhs,_) <- fmap unzip3 (mapM (t2X env' x) xs)
                                     let s = mergeSubsts ss
                                     return (subst s alpha, subst s rhs)


t2Gen env rh                    = do ids <- newNames tyvarSym (length tvs)
                                     let s = tvs `zip` map TId ids
                                     return (Scheme (subst s rh) [] (ids `zip` map tvKind tvs))
  where tvs                     = nub (filter (`notElem` tvs0) (tvars rh))
        tvs0                    = tevars env
    

t2Inst (Scheme rh ps ke)        = do ts <- mapM newTVar ks
                                     return (subst (vs `zip` ts) (tFun ps rh))
  where (vs,ks)                 = unzip ke



mgi (R t) (R u)                 = return (unif [(t,u)])
mgi (F ts t) (F us u)           = do s <- mgi t u
                                     ss <- mapM mgiSc (us `zip` ts)
                                     return (mergeSubsts (s:ss))
mgi (R (TFun ts t)) rh          = mgi (F (map scheme ts) (R t)) rh
mgi rh (R (TFun us u))          = mgi rh (F (map scheme us) (R u))
mgi (R t) (F us u)              = do (t':ts) <- mapM newTVar (replicate (length us + 1) Star)
                                     let s1 = unif [(t,TFun ts t')]
                                     s2 <- mgi (R (subst s1 t)) (F us u)
                                     return (s2@@s1)
mgi (F ts t) (R u)              = do (u':us) <- mapM newTVar (replicate (length ts + 1) Star)
                                     let s1 = unif [(u,TFun us u')]
                                     s2 <- mgi (F ts t) (R (subst s1 u))
                                     return (s2@@s1)


mgiSc (Scheme rh [] [], Scheme rh' [] [])
                                = mgi rh rh'
mgiSc (sc, sc')                 = do t <- t2Inst sc
                                     t' <- t2Inst sc'           -- Note: don't skolemize, use unification variables instead,
                                     mgi t t'                  -- since no unification errors should appear in this pass anyway


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
unif eqs                        = internalError0 ("Type2.unif " ++ show eqs)


mergeSubsts ss                  = unif (mapFst TVar (concat ss))
