module Syntax2Core where


import Common
import Syntax
import Monad
import qualified Core


syntax2core m = s2c m


-- translate a module in the empty environment
s2c                             :: Module -> M Core.Module
s2c (Module v ds)               = do (ts,pe,bs) <- s2cDecls env0 ds [] [] [] []
                                     return (Core.Module v ts pe bs)
  where env0                    = Env { sigs = [] }


-- type signature environments
data Env                        = Env { sigs :: Map Name Type }

addSigs te env                  = env { sigs = te ++ sigs env }


-- Syntax to Core translation of declarations ========================================================

-- translate top-level declarations, accumulating signature environment env, kind environment ke, 
-- type declarations ts, predicate environment (instance signatures) pe, as well as bindings bs
s2cDecls env [] ke ts pe bs     = do (te,bs2) <- s2cBinds (addSigs pe env) te es2
                                     (_ ,bs1) <- s2cBinds (addSigs te env) pe es1
                                     ke' <- mapM s2cKSig (impl_ke `zip` repeat KWild)
                                     let ds = Core.Types (reverse ke ++ ke') (reverse ts)
                                     return (ds, bs1, bs2)
  where (te,es)                 = splitBinds (reverse bs)
        (es1,es2)               = partition ((`elem` ws) . fst) es
        impl_ke                 = dom ts \\ dom ke
        ws                      = doms2cSigpe


s2cDecls env (DKSig c k : ds) ke ts pe bs
                                = do ck  <- s2cKSig (c,k)
                                     s2cDecls env ds (ck:ke) ts pe bs
s2cDecls env (DData c vs bts cs : ds) ke ts pe bs
                                = do bts <- mapM s2cQualType bts
                                     cs  <- mapM s2cConstr cs
                                     s2cDecls env' ds ke ((c,Core.DData vs bts cs):ts) pe bs
  where env'                    = addSigs (teConstrs c vs cs) env
s2cDecls env (DRec isC c vs bts ss : ds) ke ts pe bs
                                = do bts <- mapM s2cQualType bts
                                     sss <- mapM s2cSig ss
                                     s2cDecls env'' ds ke ((c,Core.DRec isC vs bts (concat sss)):ts) pe bs
  where env'                    = addSigs (teSigs c vs ss) env
        env''                   = if isC then addSigs (teMems c vs ss) env' else env'
s2cDecls env (DType c vs t : ds) ke ts pe bs  
                                = do t <- s2cType t
                                     s2cDecls env ds ke ((c,Core.DType vs t):ts) pe bs
s2cDecls env (DPSig w p : ds) ke ts pe bs  
                                = s2cDecls env ds ke ts ((w,p):pe) bs 
s2cDecls env (DBind b : ds) ke ts pe bs
                                = s2cDecls env ds ke ts pe (b:bs)

--translate a constructor declaration
s2cConstr (Constr c ts ps)      = do ts <- mapM s2cQualType ts
                                     (qs,ke) <- s2cQuals ps
                                     return (c, Core.Constr ts qs ke)


-- translate a field declaration
s2cSig (Sig vs qt)              = s2cTSig vs qt


-- add suppressed wildcard kind  
pVar v                          = PKind v KWild


-- buld a signature environment for data constructors cs in type declaration tc vs
teConstrs tc vs cs              = map f cs
  where t0                      = foldl TAp (TCon tc) (map TVar vs)
        f (Constr c ts ps)      = (c,TQual (tFun ts t0) (ps ++ map pVar vs))

-- build a signature environment for record selectors ss in type declaration tc vs
teSigs tc vs ss                 = concat (map f ss)
  where t0                      = foldl TAp (TCon tc) (map TVar vs)
        f (Sig ws (TQual t ps)) = ws `zip` repeat (TQual (TFun [t0] t) (ps ++ map pVar vs))
        f (Sig ws t)            = ws `zip` repeat (TQual (TFun [t0] t) (map pVar vs))

-- build a signature environment for type class members ss in class declaration tc vs
teMems tc vs ss                 = concat (map f ss)
  where p0                      = PType (foldl TAp (TCon tc) (map TVar vs))
        f (Sig ws (TQual t ps)) = ws `zip` repeat (TQual t (p0 : ps ++ map pVar vs))
        f (Sig ws t)            = ws `zip` repeat (TQual t (p0 : map pVar vs))


-- Signatures =========================================================================

-- translate a type signature
s2cTSig vs t                    = do ts <- mapM (s2cQualType . const t) vs
                                     return (vs `zip` ts)


-- Types ==============================================================================

-- translate a qualified type scheme
s2cQualType (TQual t qs)        = do (ps,ke) <- s2cQuals qs
                                     t <- s2cRhoType t
                                     return (Core.Scheme t ps ke)

-- translate a rank-N function type
s2cRhoType (TFun ts t)          = do ts <- mapM s2cQualType ts
                                     t <- s2cRhoType t
                                     return (Core.F ts t)
s2cRhoType t                    = liftM Core.R (s2cType t)


-- translate a monomorphic type
s2cType (TFun ts t)             = do ts <- mapM s2cType ts
                                     t <- s2cType t
                                     return (Core.TFun ts t)
s2cType (TAp t t')              = liftM2 Core.TAp (s2cType t) (s2cType t')


s2cType (TCon c)                = return (Core.TId c)
s2cType (TVar v)                = return (Core.TId v)
s2cType (TWild)                 = do k <- newKVar
                                     Core.newTVar k


-- translate qualifiers, separating predicates from kind signatures along the way
s2cQuals qs                     = s2c [] [] qs
  where
    s2c ps ke []                = return (reverse ps, reverse ke)
    s2c ps ke (PKind v k : qs)  = do k <- s2cKind k
                                     s2c ps ((v,k) : ke) qs
    s2c ps ke (PType t : qs)    = do p <- s2cQualType t
                                     s2c (p : ps) ke qs


-- Kinds ==============================================================================

-- translate a kind
s2cKind Star                    = return Star
s2cKind KWild                   = newKVar
s2cKind (KFun k k')             = liftM2 KFun (s2cKind k) (s2cKind k')

s2cKSig (v,k)                   = do k <- s2cKind k
                                     return (v,k)

-- Expressions and bindings ============================================================

-- the translated default case alternative
dflt                                    = Core.EVar (prim Fail)


-- translate a case alternative, inheriting type signature t top-down
s2cA env t (Alt (ELit l) (RExp e))      = do e' <- s2cEc env t e
                                             return (Core.PLit l, e')
s2cA env t (Alt (ECon c) (RExp e))      = do e' <- s2cEc env (TFun ts t) e
                                             return (Core.PCon c, e')
  where ts                              = splitArgs (lookupT c env)


-- translate a record field
s2cF env (Field s e)                    = do e <- s2cEc env (snd (splitT (lookupT s env))) e
                                             return (s,e)


-- split bindings bs into type signatures and equations
splitBinds bs                           = s2cB [] [] bs
  where 
    s2cB sigs eqs []                    = (reverse sigs, reverse eqs)
    s2cB sigs eqs (BSig vs t : bs)      = s2cB (vs `zip` repeat t ++ sigs) eqs bs
    s2cB sigs eqs (BEqn (LFun v []) (RExp e) : bs)
                                        = s2cB sigs ((v,e):eqs) bs


-- translate equation eqs in the scope of corresponding signatures sigs
s2cBinds env sigs eqs                   = do (ts,es) <- fmap unzip (mapM s2cEqn eqs)
                                             let te = vs `zip` ts
                                             te' <- s2cTE te
                                             return (te, Core.Binds True te' (vs `zip` es))
  where vs                              = dom eqs
        env'                            = addSigs sigs env
        s2cEqn (v,e)                    = case lookup v sigs of
                                            Nothing -> s2cEi env' e
                                            Just t  -> do e <- s2cEc env' (peel t) e
                                                          return (t,e)


-- Expressions, inherit mode ===============================================================

-- translate an expression, inheriting type signature t top-down
s2cEc env t (ELam ps e)         = do e' <- s2cEc (addSigs te env) t' e
                                     te' <- s2cTE te
                                     return (Core.ELam te' e')
  where (te,t')                 = mergeT ps t
s2cEc env _ (EAp e1 e2)         = do (t,e1) <- s2cEi env e1
                                     let (t1,_) = splitT t
                                     e2 <- s2cEc env (peel t1) e2
                                     return (Core.eAp e1 [e2])
s2cEc env t (ELet bs e)         = do (te',bs') <- s2cBinds env te eqs
                                     e' <- s2cEc (addSigs te' env) t e
                                     return (Core.ELet bs' e')
  where (te,eqs)                = splitBinds bs
s2cEc env t (ECase e alts)      = do e <- s2cEc env TWild e
                                     alts <- mapM (s2cA env t) alts
                                     return (Core.ECase e alts dflt)
s2cEc env t (ESelect e s)       = do e <- s2cEc env (peel t1) e
                                     return (Core.eAp (Core.ESel s) [e])
  where (t1,_)                  = splitT (lookupT s env)
s2cEc env t (ECon c)            = return (Core.ECon c)
s2cEc env t (EVar v)            = return (Core.EVar v)
s2cEc env t (ESel s)            = return (Core.ESel s)
s2cEc env t (ELit l)            = return (Core.ELit l)
s2cEc env _ e                   = s2cE env e


-- Expressions, agnostic mode ================================================================

-- translate an expression whose type cannot be rank-N polymorphic 
-- (i.e., no point inheriting nor synthesizing signatures)
s2cE env (ERec _ fs)                    = do eqs <- mapM (s2cF env) fs
                                             return (Core.ERec eqs)
s2cE env (EAct x ss)                    = do c <- s2cS env ss
                                             return (Core.EAct x c)
s2cE env (EReq x ss)                    = do c <- s2cS env ss
                                             return (Core.EReq x c)
s2cE env (EDo ss)                       = do c <- s2cS env ss
                                             return (Core.EDo c)
s2cE env (ETempl x ss)                  = do c <- s2cS (addSigs te env) (map unsig ss)
                                             te2 <- s2cTE te1
                                             return (Core.ETempl x te2 Nothing c)
  where 
    vs                                  = svars ss
    te                                  = sigs ss
    te1                                 = prune te vs ++ ((vs \\ dom te) `zip` repeat TWild)

    sigs []                             = []
    sigs (SGen (ESig (EVar v) t) _ :ss) = (v,t) : sigs ss
    sigs (SAss (ESig (EVar v) t) _ :ss) = (v,t) : sigs ss
    sigs (SBind (BSig vs t) : ss)       = vs `zip` repeat t ++ sigs ss
    sigs (_ : ss)                       = sigs ss

    unsig (SAss (ESig p t) e)           = SAss p e
    unsig s                             = s

s2cE env e                              = error ("Internal s2cE " ++ show e)


-- Statements ==================================================================================

-- translate a statement list
s2cS env []                             = return (Core.CRet (Core.ECon (prim UNIT)))
s2cS env [SRet e]                       = do (t,e') <- s2cEi env e
                                             return (Core.CRet e')
s2cS env [SExp e]                       = do (t,e') <- s2cEi env e
                                             return (Core.CExp e')
s2cS env (SGen (ESig (EVar v) t) e :ss) = do t' <- s2cQualType t
                                             e' <- s2cEc env TWild e
                                             c <- s2cS (addSigs [(v,t)] env) ss
                                             return (Core.CGen v t' e' c)
s2cS env (SGen (EVar v) e : ss)         = do (t,e') <- s2cEi env e
                                             t' <- s2cQualType t
                                             c <- s2cS env ss
                                             return (Core.CGen v t' e' c)
s2cS env (SAss (ESig (EVar v) t) e :ss) = fail ("Illegal state variable signature: " ++ show v)
s2cS env (SAss (EVar v) e : ss)         = do e' <- s2cEc env (lookupT v env) e
                                             c <- s2cS env ss
                                             return (Core.CAss v e' c)
s2cS env (SBind b : ss)                 = s2cS' env [b] ss


-- translate a binding group prefix of a statement list
s2cS' env bs (SBind b : ss)             = s2cS' env (b:bs) ss
s2cS' env bs ss                         = do (te',bs') <- s2cBinds env te es
                                             c <- s2cS (addSigs te' env) ss
                                             return (Core.CLet bs' c)
  where (te,es)                         = splitBinds (reverse bs)


-- Expressions, synthesize mode ================================================================

-- translate an expression, synthesize type signature bottom-up
s2cEi env (ELam ps e)           = do (t,e') <- s2cEi (addSigs te env) e
                                     te' <- s2cTE te
                                     return (TFun (rng te) t, Core.ELam te' e')
  where (te,_)                  = mergeT ps TWild
s2cEi env (EAp e1 e2)           = do (t,e1) <- s2cEi env e1
                                     let (t1,t2) = splitT t
                                     e2 <- s2cEc env (peel t1) e2
                                     return (t2, Core.eAp e1 [e2])
s2cEi env (ELet bs e)           = do (te',bs') <- s2cBinds env te eqs
                                     (t,e') <- s2cEi (addSigs te' env) e
                                     return (t, Core.ELet bs' e')
  where (te,eqs)                = splitBinds bs
s2cEi env (ECase e alts)        = do e <- s2cEc env TWild e
                                     alts <- mapM (s2cA env TWild) alts
                                     return (TWild, Core.ECase e alts dflt)
s2cEi env (ESelect e s)         = do e <- s2cEc env (peel t1) e
                                     return (t2, Core.eAp (Core.ESel s) [e])
  where (t1,t2)                 = splitT (lookupT s env)
s2cEi env (ESel s)              = return (lookupT s env, Core.ESel s)
s2cEi env (ECon c)              = return (lookupT c env, Core.ECon c)
s2cEi env (EVar v)              = return (lookupT v env, Core.EVar v)
s2cEi env (ELit l)              = return (TWild, Core.ELit l)
s2cEi env e                     = do e' <- s2cE env e
                                     return (TWild, e')


-- Misc ====================================================================================

-- translate type enviroment te
s2cTE te                        = mapM s2cVT te
  where s2cVT (v,t)             = do t <- s2cQualType t
                                     return (v,t)


-- split a function type into domain (one parameter only) and range
-- if not a function type, fail gracefully (error will be caught later by real type-checker)
splitT (TFun [t] t')            = (t, t')
splitT (TFun (t:ts) t')         = (t, TFun ts t')
splitT TWild                    = (TWild,TWild)
splitT _                        = (TWild,TWild)


-- return the domain of a function type (all immediate parameters)
splitArgs (TFun ts t)           = ts
splitArgs t                     = []


-- merge any signatures in patterns ps with domain of type t, pair with range of t
mergeT ps t                     = (zipWith f ts ps, t')
  where (ts,t')                 = split (length ps) t
        f t (EVar v)            = (v,t)
        f t (ESig (EVar v) t')  = (v,t')
        f t e                   = error ("Internal: mergeT: " ++ show e)
        split 0 t               = ([],t)
        split n t               = (t1:ts,t')
          where (t1,t2)         = splitT t
                (ts,t')         = split (n-1) t2


-- find and instantate the type signature for x if present, otherwise return _
lookupT x env                   = case lookup x (sigs env) of
                                    Nothing -> TWild
                                    Just t  -> peel t


-- peel off the outermost qualifiers of a type, replacing all bound variables with _
peel (TQual t ps)               = g (bvars ps) t
  where g vs (TQual t ps)       = TQual (g vs' t) (map (h vs') ps)
          where vs'             = vs \\ bvars ps
        g vs (TAp t t')         = TAp (g vs t) (g vs t')
        g vs (TFun ts t)        = TFun (map (g vs) ts) (g vs t)
        g vs (TSub t t')        = TSub (g vs t) (g vs t')
        g vs (TList t)          = TList (g vs t)
        g vs (TTup ts)          = TTup (map (g vs) ts)
        g vs (TVar v)
          | v `elem` vs         = TWild
        g vs t                  = t
        h vs (PType t)          = PType (g vs t)
        h vs p                  = p
peel t                          = t


