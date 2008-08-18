module Type2 where
    
import Common
import Core
import Env
import Decls
import PP

typecheck2 e2 m                 = t2Module e2 m

t2Module (xs',ds',ws',bs') (Module v ns xs ds ws bss)
                                = do bss <- t2BindsList env2 bss
                                     return (Module v ns xs ds ws bss)
  where env2                    = addTEnv0 te2 (addKEnv0 ke2 env1)
        te2                     = tenvSelsCons ds
        ke2                     = ksigsOf ds
        env1                    = addTEnv0 te1 (addKEnv0 ke1 env0)
        te1                     = tsigsOf bs' ++ tenvSelsCons ds'
        ke1                     = ksigsOf ds'
        env0                    = addTEnv0 primPredEnv (initEnv v)

t2BindsList env []              = return []
t2BindsList env (bs:bss)        = do bs <- t2Binds0 env bs
                                     bss <- t2BindsList (addTEnv (tsigsOf bs) env) bss
                                     return (bs:bss)

-- t2Binds0 is a variant of t2Binds that is optimized for use on the program top level, where the only free 
-- occurrences of unification variables are in partial type signatures.  After type-checking a right-hand side,
-- all unification variables that still remain will be generalized before a dependent binding group is checked.
-- Thus there is no need to propagate any substitution from one top-level binding group to the next; and
-- furthermore, the only fragment of a substitution that can possibly affect the type of a binding whithin
-- the *same* group must have the free unification variables of the corresponding type signature as its domain.
-- Hence the restriction of s1 to the tvars of sc below.  Note: this optimization has considerable impact on
-- the efficiency of the Type2 pass!
-- Another difference between t2Binds0 and t2Binds is that t2Binds0 also applies the computed substitution to
-- the top-level *term* of a binding, so that nested signatures inside the right-hand side term may be properly 
-- updated.  Note that this is done as early as possible (immediately after a right-hand side has been checked),
-- in order to allow the computed substitution to be restricted before it is applied to any sibling bindings.

t2Binds0 env (Binds r te eqs)   = do (s,eqs') <- t2Eqs eqs
                                     scs' <- mapM (t2Gen env) (subst s scs)
                                     return (Binds r (xs `zip` scs') eqs')
  where env1                    = if r then addTEnv te env else env
        (xs,scs)                = unzip te
        t2Eqs []                = return (nullSubst, [])
        t2Eqs ((x,e):eqs)       = do (s1,e) <- t2ExpTscoped env1 sc e
                                     (s2,eqs) <- t2Eqs eqs
                                     return (mergeSubsts [restrict s1 (tvars sc),s2], (x, subst s1 e):eqs)
          where sc              = lookup' te x


t2Binds env (Binds r te eqs)    = do (s,eqs') <- t2Eqs eqs
                                     scs' <- mapM (t2Gen (subst s env)) (subst s scs)
                                     return (s, Binds r (xs `zip` scs') eqs')
  where env1                    = if r then addTEnv te env else env
        (xs,scs)                = unzip te
        t2Eqs []                = return (nullSubst, [])
        t2Eqs ((x,e):eqs)       = do (s1,e) <- t2ExpTscoped env1 sc e
                                     (s2,eqs) <- t2Eqs eqs
                                     return (mergeSubsts [s1,s2], (x,e):eqs)
          where sc              = lookup' te x

-- Note: the program is known to be typeable at this point, thus there is no need to generalize, freshly 
-- instantiate, and then match an inferred type against a skolemized version of the expected type scheme 
-- (together with checking for escaping skolem variables).  Instead, all we need to ensure is that any 
-- type equalities implied by the match are captured in the resulting substitution, treating all-quantified 
-- variables as scoped constants once we are inside the scope of a type signature (t2ExpTscoped). Moreover, 
-- in order to ensure that all true polymorphic terms are properly marked, t2ExpT wraps them inside a 
-- let-binding with an explicit signature whenever appropriate.

t2ExpTscoped env sc e           = do (s1,rh,e) <- t2Exp env e
                                     s2 <- mgi rh (quickSkolem sc)
                                     return (mergeSubsts [s1,s2], e)
                                     

t2ExpT env (Scheme t qs []) e   = t2ExpTscoped env (Scheme t qs []) e
t2ExpT env sc (EVar x)          = t2ExpTscoped env sc (EVar x)
t2ExpT env sc e                 = do sc' <- ac nullSubst sc
                                     (s,e') <- t2ExpTscoped env sc' e
                                     x <- newName tempSym
                                     return (s, ELet (Binds False [(x,sc)] [(x,e)]) (EVar x))


t2ExpTs env [] []               = return (nullSubst, [])
t2ExpTs env (sc:scs) (e:es)     = do (s1,e) <- t2ExpT env sc e
                                     (s2,es) <- t2ExpTs env scs es       
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
                                     (s,e) <- t2ExpT env sc e
                                     return (s, subst s (tFun scs rh), ESel e l)
t2Exp env (ELam te e)           = do (s,rh,e) <- t2Exp (addTEnv te env) e
                                     return (s, F (subst s (rng te)) rh, ELam te e)
t2Exp env (EAp e es)            = do (s,rh,e) <- t2Exp env e
                                     t2Ap env s rh e es
t2Exp env (ELet bs e)           = do (s1,bs) <- t2Binds env bs
                                     (s2,rh,e) <- t2Exp (addTEnv (subst s1 (tsigsOf bs)) env) e
                                     return (mergeSubsts [s1,s2], rh, ELet bs e)
t2Exp env (ERec c eqs)          = do alphas <- mapM newTVar (kArgs (findKind env c))
                                     (t,scs) <- t2Lhs env (foldl TAp (TId c) alphas) t2Sel ls
                                     (s,es) <- t2ExpTs env scs es
                                     return (s, R (subst s t), ERec c (ls `zip` es))
  where (ls,es)                 = unzip eqs
        t2Sel env x l           = t2Exp env (ESel (EVar x) l)
t2Exp env (ECase e alts)        = do alpha <- newTVar Star
                                     (TFun [t0] t1,scs) <- t2Lhs env alpha t2Pat ps
                                     (s0,e) <- t2ExpT env (scheme t0) e
                                     (s1,es) <- t2ExpTs env scs es
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
                                     (s1,e1) <- t2ExpT env (scheme (tRef alpha)) e1
                                     (s2,e2) <- t2ExpT env (scheme (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R (tRequest (subst s beta)), EReq e1 e2)
t2Exp env (EAct e1 e2)          = do alpha <- newTVar Star
                                     beta <- newTVar Star
                                     (s1,e1) <- t2ExpT env (scheme (tRef alpha)) e1
                                     (s2,e2) <- t2ExpT env (scheme (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R tAction, EAct e1 e2)
t2Exp env (EDo x tx c)          = do (s1,t,c) <- t2Cmd (setSelf x tx env) c
                                     let s2 = case stateT env of Nothing -> nullSubst; Just t' -> unif [(t',tx)]
                                         s = mergeSubsts [s1,s2]
                                     return (s, R (subst s (tCmd tx t)), EDo x tx c)
t2Exp env (ETempl x tx te c)    = do (s,t,c) <- t2Cmd (setSelf x tx (addTEnv te env)) c
                                     return (s, R (tClass t), ETempl x tx te c)

        
t2Cmd env (CRet e)              = do alpha <- newTVar Star
                                     (s,e) <- t2ExpT env (scheme alpha) e
                                     return (s, subst s alpha, CRet e)
t2Cmd env (CExp e)              = do alpha <- newTVar Star
                                     (s,e) <- t2ExpT env (scheme (tCmd (fromJust (stateT env)) alpha)) e
                                     return (s, subst s alpha, CExp e)
t2Cmd env (CGen x tx e c)       = do (s1,e) <- t2ExpT env (scheme (tCmd (fromJust (stateT env)) tx)) e
                                     (s2,t,c) <- t2Cmd (addTEnv [(x,scheme tx)] env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CGen x tx e c)
t2Cmd env (CAss x e c)          = do (s1,e) <- t2ExpT env (findType env x) e
                                     (s2,t,c) <- t2Cmd env c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CAss x e c)
t2Cmd env (CLet bs c)           = do (s1,bs) <- t2Binds env bs
                                     (s2,t,c) <- t2Cmd (addTEnv (tsigsOf bs) env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CLet bs c)

                                     

t2Ap env s1 (F scs rh) e es     = do (s2,es) <- t2ExpTs env scs es
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
                                     scs <- mapM (t2Gen (subst s env') . scheme') (subst s rhs)
                                     return (subst s alpha, scs)


t2Gen env (Scheme rh ps ke)     = do ids <- newNames tyvarSym (length tvs)
                                     let s = tvs `zip` map TId ids
                                     return (Scheme (subst s rh) ps (ke ++ ids `zip` map tvKind tvs))
  where tvs                     = nub (filter (`notElem` tvs0) (tvars rh))
        tvs0                    = tevars env
    

t2Inst (Scheme rh ps ke)        = do ts <- mapM newTVar ks
                                     return (subst (vs `zip` ts) (tFun ps rh))
  where (vs,ks)                 = unzip ke


quickSkolem (Scheme rh ps ke)   = tFun ps rh


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
mgiSc (sc, sc')                 = do rh <- t2Inst sc
                                     mgi rh (quickSkolem sc')


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
