module Core2Kindle(core2kindle) where
{- -}
import List(unzip4)
import Common
import Core
import Name
import Depend
import qualified Kindle


core2kindle                         :: Module -> M Kindle.Module
core2kindle m                       = cModule m



data Env                            = Env { decls :: Kindle.Decls,
                                            tenv  :: Kindle.TEnv,
                                            dict  :: Map Name Name,
                                            self  :: Name }

env0                                = Env { decls = Kindle.primDecls, 
                                            tenv  = [],
                                            dict  = Kindle.primDict,
                                            self  = name0 "" }

invdict env                         = inv (dict env)

addDecls ds env                     = env { decls = ds ++ decls env }

addTEnv te env                      = env { tenv = te ++ tenv env }

addDict d env                       = env { dict = d ++ dict env }

pushSelf x t env                    = env { tenv = (x,Kindle.ValT t) : tenv env, self = x }

findSel env l                       = head [ (Kindle.TId n, t) | (n, Kindle.Struct te) <- decls env, (l',t) <- te, l==l' ]

findCon env k                       = case lookup k (decls env) of
                                        Just (Kindle.Struct te) -> (Kindle.TId n, Just (mapSnd valT te))
                                        _                       -> (Kindle.TId n, Nothing)
  where k'                          = substVar (dict env) k
        n'                          = head [ c | (c, Kindle.Enum cs) <- decls env, k' `elem` cs ]
        n                           = substVar (invdict env) n'
        valT (Kindle.ValT t)        = t
        valT (Kindle.FunT _ _)      = error "Internal: c2k.findCon"     -- can't happen, see cCon below


-- Convert a Core.Modul into a Kindle.Module
cModule (Module m ds _ bs)          = do env <- conDict ds
                                         ds <- cDecls env ds
                                         (ds',bs) <- cBindsList (addDecls ds env) (groupBinds bs)
                                         return (Kindle.Module m (ds++ds') bs)

-- Construct the required renaming dictionary on basis of the declared data constructors
conDict (Types ke ds)               = do d <- renaming cons
                                         return (env0 { dict = d })
  where cons                        = concat [ c : dom cs | (c,DData _ _ cs) <- ds, not (all simple cs) ]


simple (c,Constr ts ps _)           = null ts && null ps


-- Convert a Core.Types into a Kindle.Decls
cDecls env (Types ke ds)            = do dss <- mapM cDecl ds
                                         return (concat dss)
  where cDecl (c,DRec _ vs [] ss)   = do (ds,te) <- cTEnv ss
                                         return ((c, Kindle.Struct te) : ds)
        cDecl (c,DType vs t)        = error ("Internal: cannot yet compile type synonyms ")
        cDecl (c,DData vs [] cs)
          | all simple cs           = return [(c, Kindle.Enum (dom cs))]
          | otherwise               = do dss <- mapM (cCon t) cs
                                         tag <- newName tagSym
                                         return ((c', Kindle.Enum cs') : (c, Kindle.Struct [(tag,t)]) : concat dss)
          where c'                  = substVar (dict env) c
                t                   = Kindle.ValT (Kindle.TId c')
                cs'                 = substVars (dict env) (dom cs)

        cCon t (c,Constr ts ps _)   = do tag <- newName tagSym
                                         te <- newEnv paramSym (ps++ts)
                                         (ds,te) <- cTEnv' te
                                         return (ds ++ [(c, Kindle.Struct ((tag,t):te))])

-- Convert a Core.TEnv into a Kindle.TEnv on basis of type arity
cTEnv te                            = do (dss,te') <- fmap unzip (mapM conv te)
                                         return (concat dss, te')
  where conv (x,t)                  = do (ds, ts, t') <- cType (deQualify t)
                                         case ts of
                                           [] -> return (ds, (x, Kindle.ValT t'))
                                           _  -> return (ds, (x, Kindle.FunT ts t'))

-- Convert a Core.TEnv into a Kindle.TEnv with only atomic types
cTEnv' te                           = do (ds,te') <- cATEnv te
                                         return (ds, mapSnd Kindle.ValT te')



-- Convert a Core.TEnv into a Kindle.ATEnv
cATEnv te                             = do (dss,te) <- fmap unzip (mapM conv te)
                                           return (concat dss, te)
  where conv (x,s)                    = do (ds,t) <- cAType (deQualify s)
                                           return (ds, (x,t))


-- Reduce schemes and rho types to ordinary (function) types by simply removing the quantifiers, leaving the tyvars free.
deQualify (Scheme rh [] ke)         = deQualify' rh
deQualify (Scheme rh ps ke)         = case deQualify' rh of
                                        TFun ts t -> TFun (map deQualify ps ++ ts) t    -- concatenate ps with topmost params
                                        t         -> TFun (map deQualify ps) t
                                        

deQualify' (R t)                    = t
deQualify' (F ts rh)                = TFun (map deQualify ts) (deQualify' rh)


-- Convert a Core.Type into a (possibly empty) list of Kindle.AType parameters and a return Kindle.AType
cType (TFun ts t)                   = do (dss,ts) <- fmap unzip (mapM cAType ts)
                                         (ds,ts',t) <- cType' t
                                         return (ds++concat dss, ts++ts', t)            -- concatenate ts with any monadic params
cType t                             = cType' t


cType' (TId (Prim Action _))              = return ([], [t,t], Kindle.TId (prim Msg))
  where t                                 = Kindle.TId (prim Time)
cType' (TAp (TId (Prim Request _)) t)     = do (ds,t) <- cAType t
                                               return (ds, [Kindle.TWild], t)
cType' (TAp (TId (Prim Template _)) t)    = do (ds,t) <- cAType t
                                               return (ds, [Kindle.TWild], t)
cType' (TAp (TAp (TId (Prim Cmd _)) s) t) = do (ds1, s) <- cAType s
                                               (ds2, t) <- cAType t
                                               return (ds1++ds2, [s], t)
cType' (TAp (TId (Prim Ref _)) t)         = do (ds,t) <- cAType t
                                               return (ds, [], t)
cType' (TAp t t')                         = cType' t
cType' (TId n)
  | isCon n                               = return ([], [], Kindle.TId n)
  | otherwise                             = return ([], [], Kindle.TWild)
cType' (TVar _)                           = error "Internal error: TVar in Core2Kindle"


-- Convert a Core.Type into a Kindle.AType, overflowing into a list of Kindle.Decls if necessary
cAType t                              = do (ds, ts, t) <- cType t
                                           case ts of
                                             [] -> return (ds, t)
                                             ts -> do c <- newName closureSym
                                                      f <- newName codeSym
                                                      return ((c, Kindle.Struct [(f,Kindle.FunT ts t)]) : ds, Kindle.TId c)


-- Convert a list of strongly connected Core binding groups into a list of Kindle bindings
cBindsList env []                     = return ([], [])
cBindsList env (bs:bss)               = do (ds1,bs1) <- cBinds env bs
                                           (ds2,bs2) <- cBindsList (addTEnv (Kindle.mkTEnv bs1) env) bss
                                           return (ds1++ds2, bs1++bs2)

-- Convert a (mutually recursive) set of Core bindings into a list of Kindle bindings on basis of declared type
cBinds env (Binds r te eqs)           = do (ds,te) <- cTEnv te
                                           assert (not r || all Kindle.isFunT te) "Illegal value recursion"
                                           (ds',bs,bs') <- cEqs (if r then addTEnv te env else env) te eqs
                                           return (ds++ds', bs++bs')


-- Convert a set of Core equations into a list of Kindle bindings on basis of declared type
cEqs env te eqs                       = do (dss,bss,bs) <- fmap unzip3 (mapM cEq eqs)
                                           return (concat dss, concat bss, bs)
  where cEq (x,e)                     = case lookup' te x of
                                          Kindle.ValT t -> do
                                              (ds,bs,e) <- cExpT env t e
                                              return (ds, bs, (x, Kindle.Val t e))
                                          Kindle.FunT ts t -> do
                                              (ds,te,c) <- cFunT env ts t e             -- Top-down mode!
                                              return (ds, [], (x, Kindle.Fun t te c))


-- =========================================================================================
-- Top-down mode
-- =========================================================================================

-- Convert a Core.Exp with an expected Kindle function type into a parameter list and a Kindle.Cmd
cFunT env ts t0 (ELam te e)
  | l_ts < l_te                         = error "Internal: c2k.cFunT"
  | l_ts >= l_te                        = do (ds,te',c) <- cFunT (addTEnv (Kindle.mkTEnv' te1) env) ts2 t0 e
                                             return (ds, te1++te', c)
  where te1                             = dom te `zip` ts1
        (ts1,ts2)                       = splitAt (l_te) ts
        l_ts                            = length ts
        l_te                            = length te
cFunT env [ty] t0 (EReq e e')           = do (ds1,bs,tx,e) <- cExp env e
                                             x <- newName selfSym
                                             (ds2,c) <- cCmdExpT (pushSelf x tx env) t0 e'
                                             c <- Kindle.protect x t0 c
                                             y <- newName dummySym
                                             return (ds1++ds2, [(y,ty)], Kindle.CBind (bs++[(x,Kindle.Val tx e)]) c)
cFunT env [_,_] t0 e@(EAct _ _)         = cAct env id id e
cFunT env [_,_] t0 e@(EAp (EVar (Prim After _) _) _) 
                                        = cAct env id id e
cFunT env [_,_] t0 e@(EAp (EVar (Prim Before _) _) _) 
                                        = cAct env id id e
cFunT env [ty] t0 (ETempl x tx te c)    = do (ds0,tx@(Kindle.TId n)) <- cAType tx     -- Type-checker guarantees tx is a struct type name
                                             (ds1,te) <- cTEnv' te
                                             (ds,c) <- cCmdT (pushSelf x tx (addTEnv te env)) t0 c
                                             y <- newName dummySym
                                             let c' = Kindle.CBind [(x,Kindle.Val tx (Kindle.ENew n []))] c
                                             return ((n,Kindle.Struct te):ds0++ds1++ds, [(y,ty)], c')
cFunT env [tx] t0 (EDo x tx' c)         = do (ds,c) <- cCmdT (pushSelf x tx env) t0 c
                                             return (ds, [(x,tx)], c)
cFunT env ts t0 e                       = do te <- newEnv paramSym ts
                                             (ds,c) <- cBodyT (addTEnv (Kindle.mkTEnv' te) env) t0 (eAp e (map eVar (dom te)))
                                             return (ds, te, c)


-- Convert a Core (Pat,Exp) pair with an expected Kindle.AType and a scrutinee expression into Kindle.Alt
cAltT cBodyT env t0 e0 (PLit l, e)      = do (ds,c) <- cBodyT env t0 e
                                             return (ds, Kindle.ALit l c)
cAltT cBodyT env t0 e0 (PCon n, e)      = case findCon env n of
                                            (_,Just te) -> do let e1 = Kindle.ECast (Kindle.TId n) e0
                                                                  (xs,ts) = unzip (tail te)
                                                              (ds,c) <- cRhsT env t0 e ts (map (Kindle.ESel e1) xs)
                                                              return (ds, Kindle.ACon (substVar (dict env) n) c)
                                            (_,Nothing) -> do (ds,c) <- cBodyT env t0 e
                                                              return (ds, Kindle.ACon (substVar (dict env) n) c)
  where cRhsT env t0 e [] []            = cBodyT env t0 e
        cRhsT env t0 (ELam te e) ts es  = do (ds,te) <- cATEnv te
                                             let bs = mkBind te (zipWith3 match (rng te) ts es)
                                             (ds',c) <- cBodyT (addTEnv (Kindle.mkTEnv bs) env) t0 e
                                             return (ds++ds', Kindle.CBind bs c)
        cRhsT env t0 e ts es            = do te <- newEnv paramSym ts
                                             let bs = mkBind te es
                                             (ds,c) <- cBodyT (addTEnv (Kindle.mkTEnv bs) env) t0 e
                                             return (ds, Kindle.CBind bs c)

      
-- Convert a Core.Exp with an expected Kindle.AType into a Kindle.Cmd
cBodyT env t0 (ELet bs e)               = do (ds,bs') <- cBindsList env (groupBinds bs)
                                             (ds',c) <- cBodyT (addTEnv (Kindle.mkTEnv bs') env) t0 e
                                             return (ds++ds', Kindle.CBind bs' c)
cBodyT env t0 (ECase e alts e')         = do (ds1,bs,t,e0) <- cExp env e
                                             (dss,alts) <- fmap unzip (mapM (cAltT cBodyT env t0 e0) alts)
                                             (ds2,c) <- cBodyT env t0 e'
                                             let ds = ds1 ++ concat dss ++ ds2
                                             return (ds, Kindle.cbind bs (Kindle.CSwitch (scrutinee env t e0) alts c))
cBodyT env t0 (EAp (EVar (Prim Refl _) _) [e])      = cBodyT env t0 e
cBodyT env t0 (EAp (EVar (Prim Match _) _) [e])     = cBodyT env t0 e
cBodyT env t0 (EAp (EVar (Prim Commit _) _) [e])    = cBodyT env t0 e
cBodyT env t0 (EVar (Prim Fail _) _)                = return ([], Kindle.CBreak)
cBodyT env t0 (EAp (EVar (Prim Fatbar _) _) [e,e']) = do (ds,c) <- cBodyT env t0 e
                                                         (ds',c') <- cBodyT env t0 e'
                                                         return (ds++ds', Kindle.CSeq c c')
cBodyT env t0 e                         = do (ds,bs,e) <- cExpT env t0 e
                                             return (ds, Kindle.cbind bs (Kindle.CRet e))



-- Convert a Core.Exp with a known Kindle.AType into a Core.Exp
cExpT env t0 e0                         = do (ds,bs,t,e) <- cExp env e0
                                             if matches t0 t then
                                                 return (ds, bs, match t0 t e)
                                              else
                                                 cExpT' t0 e0
  where 
    matches (Kindle.TId n) (Kindle.TId n')
      | isClosure n && isClosure n'     = length ts == length ts' && and (zipWith matches (t:ts) (t':ts'))
      where Kindle.Struct te            = lookup' (decls env) n
            Kindle.Struct te'           = lookup' (decls env) n'
            (_, Kindle.FunT ts t)       = head te
            (_, Kindle.FunT ts' t')     = head te'
    matches t t'                        = True
    cExpT' t0@(Kindle.TId n0) e
      | isClosure n0                    = do (ds,te,c) <- cFunT env ts t e
                                             n <- newName closureSym
                                             return ((n,s):ds, [], Kindle.ECast t0 (Kindle.ENew n [(x, Kindle.Fun t te c)]))
      where s@(Kindle.Struct te0)       = lookup' (decls env) n0
            (x, Kindle.FunT ts t)       = head te0


cExpTs env [] []                        = return ([], [], [])
cExpTs env (t:ts) (e:es)                = do (ds,bs,e) <- cExpT env t e
                                             (ds',bs',es) <- cExpTs env ts es
                                             return (ds++ds', bs++bs', e:es)


-- Convert a Core.Cmd into a Kindle.Cmd
cCmdT env t0 (CRet e)                   = do (ds,bs,e) <- cExpT env t0 e
                                             return (ds, Kindle.cbind bs (Kindle.CRet e))
cCmdT env t0 (CAss x e c)               = do (ds,bs,e) <- cExpT env tx e
                                             (ds',c) <- cCmdT env t0 c
                                             return (ds++ds', Kindle.cbind bs (Kindle.CAssign (Kindle.EVar (self env)) x e c))
  where tx                              = case lookup' (tenv env) x of
                                             Kindle.ValT t -> t
                                             _ -> error "Internal: c2k.cCmdT"
cCmdT env t0 (CLet bs c)                = do (ds,bs') <- cBindsList env (groupBinds bs)
                                             (ds',c) <- cCmdT (addTEnv (Kindle.mkTEnv bs') env) t0 c
                                             return (ds++ds', Kindle.CBind bs' c)
cCmdT env t0 (CGen x tx e c)
  | isDummy x                           = do (ds1,bs,t,e) <- cExp env (EAp e [eVar (self env)])
                                             (ds2,c) <- cCmdT env t0 c
                                             return (ds1++ds2, Kindle.cbind bs (Kindle.CRun e c))
  | otherwise                           = do (ds1,tx) <- cAType tx
                                             (ds2,bs,e) <- cExpT env tx (EAp e [eVar (self env)])
                                             (ds3,c) <- cCmdT (addTEnv [(x,Kindle.ValT tx)] env) t0 c
                                             return (ds1++ds2++ds3, Kindle.cbind bs (Kindle.CBind [(x,Kindle.Val tx e)] c))
cCmdT env t0 (CExp e)                   = cCmdExpT env t0 e


-- Convert a Core.Exp in the execution path into a Kindle.Cmd
cCmdExpT env t0 (EDo x tx c)            = do (ds,tx) <- cAType tx
                                             (ds',c) <- cCmdT (pushSelf x tx env) t0 c
                                             return (ds++ds', Kindle.CBind [(x,Kindle.Val tx (Kindle.EVar (self env)))] c)
cCmdExpT env t0 (ECase e alts e')       = do (ds1,bs,t,e0) <- cExp env e
                                             (dss,alts) <- fmap unzip (mapM (cAltT cCmdExpT env t0 e0) alts)
                                             (ds2,c) <- cBodyT env t0 e'
                                             let ds = ds1 ++ concat dss ++ ds2
                                             return (ds, Kindle.cbind bs (Kindle.CSwitch (scrutinee env t e0) alts c))
cCmdExpT env t0 e                       = do (ds,bs,e) <- cExpT env t0 (EAp e [eVar (self env)])
                                             return (ds, Kindle.cbind bs (Kindle.CRet e))


-- =========================================================================================
-- Misc helpers
-- =========================================================================================

-- Cast Kindle.Exp e into Kindle.AType t (if necessary)
match t t' e
  | t == t'                             = e
  | otherwise                           = Kindle.ECast t e


-- Map a Kindle.ATEnv and a list of Kindle.Exps into a list of Kindle.Binds
mkBind te es                            = zipWith f te es
  where f (x,t) e                       = (x, Kindle.Val t e)


-- Generate the operations necessary to acces the tag of a case head expression
scrutinee env (Kindle.TId n)            = case lookup n (decls env) of
                                            Just (Kindle.Struct te) -> (\e -> Kindle.ESel e (fst (head te)))
                                            Just (Kindle.Enum cs)   -> id   -- all simple contructors
                                            Nothing                 -> id   -- primitive type

-- Convert an action expression into a Core.Cmd (agnostic mode)
cAct env fa fb (EAp (EVar (Prim After _) _) [e,e'])
                                        = do (ds,bs,e1) <- cExpT env tTime e
                                             (ds',te,c) <- cAct env (sum e1 . fa) fb e'
                                             return (ds++ds', te, Kindle.cbind bs c)
  where sum e1 a                        = Kindle.ECall (prim TimePlus) [e1,a]
cAct env fa fb (EAp (EVar (Prim Before _) _) [e,e'])
                                        = do (ds,bs,e1) <- cExpT env tTime e
                                             (ds',te,c) <- cAct env fa (min e1 . fb) e'
                                             return (ds++ds', te, Kindle.cbind bs c)
  where min e1 b                        = Kindle.ECall (prim TimeMin) [e1,b]
cAct env fa fb (EAct e e')              = do (ds1,ignore_te,t,c) <- cFun env (EReq e e')
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             m  <- newName tempSym
                                             mT <- newName typeSym
                                             f  <- newName codeSym
                                             bl <- newName labelSym
                                             dl <- newName labelSym
                                             let c'  = Kindle.CBind bs (Kindle.CRun e1 (Kindle.CRet (Kindle.EVar m)))
                                                 bs  = [(m, Kindle.Val tMsg (Kindle.ECast tMsg (Kindle.ENew mT bs')))]
                                                 bs' = [(f, Kindle.Fun t [] c)]
                                                 te  = [(f,Kindle.FunT [] t), (bl,Kindle.ValT tTime), (dl,Kindle.ValT tTime)]
                                                 ds2 = [(mT, Kindle.Struct te)]
                                                 es  = [Kindle.EVar m, fa (Kindle.EVar a), fb (Kindle.EVar b)]
                                                 e1  = Kindle.ECall (prim ASYNC) es
                                             return (ds1++ds2, [(a,tTime),(b,tTime)], c')

tTime                                   = Kindle.TId (prim Time)
tMsg                                    = Kindle.TId (prim Msg)

cAct' env fa fb e                       = do (ds,te,c) <- cAct env fa fb e
                                             return (ds, te, tMsg, c)
                                             

-- =========================================================================================
-- Bottom-up mode
-- =========================================================================================

-- Convert a Core.Exp into a parameter list, a Kindle.AType and a Kindle.Cmd
cFun env (ELam te e)                    = do (ds,te) <- cATEnv te
                                             (ds',te',t,c) <- cFun (addTEnv (Kindle.mkTEnv' te) env) e
                                             return (ds++ds', te++te', t, c)
cFun env (EReq e e')                    = do (ds1,bs,tx,e) <- cExp env e
                                             x <- newName selfSym
                                             (ds2,t,c) <- cCmdExp (pushSelf x tx env) e'
                                             c <- Kindle.protect x t c
                                             y <- newName dummySym
                                             return (ds1++ds2, [(y,Kindle.TWild)], t, Kindle.CBind (bs++[(x,Kindle.Val tx e)]) c)
cFun env e@(EAct _ _)                   = cAct' env id id e
cFun env e@(EAp (EVar (Prim After _) _) _) 
                                        = cAct' env id id e
cFun env e@(EAp (EVar (Prim Before _) _) _) 
                                        = cAct' env id id e
cFun env (ETempl x tx te c)             = do (ds0,tx@(Kindle.TId n)) <- cAType tx     -- Type-checker guarantees tx is a struct type name
                                             (ds1,te) <- cTEnv' te
                                             (ds2,t,c) <- cCmd (pushSelf x tx (addTEnv te env)) c
                                             y <- newName dummySym
                                             let c' = Kindle.CBind [(x,Kindle.Val tx (Kindle.ENew n []))] c
                                             return ((n,Kindle.Struct te):ds0++ds1++ds2, [(y,Kindle.TWild)], t, c')
cFun env (EDo x tx c)                   = do (ds,tx) <- cAType tx
                                             (ds',t,c) <- cCmd (pushSelf x tx env) c
                                             return (ds++ds', [(x,tx)], t, c)
cFun env e                              = do (ds,t,c) <- cBody env e
                                             return (ds, [], t, c)


-- Convert a Core (Pat,Exp) pair and a scrutinee expression into Kindle.AType and a Kindle.Alt
cAlt cBody env e0 (PLit l, e)           = do (ds,t,c) <- cBody env e
                                             return (ds, t, Kindle.ALit l c)
cAlt cBody env e0 (PCon n, e)           = case findCon env n of
                                            (_,Just te) -> do let e1 = Kindle.ECast (Kindle.TId n) e0
                                                                  (xs,ts) = unzip (tail te)
                                                              (ds,t, c) <- cRhs env e ts (map (Kindle.ESel e1) xs)
                                                              return (ds, t, Kindle.ACon (substVar (dict env) n) c)
                                            (_,Nothing) -> do (ds,t,c) <- cBody env e
                                                              return (ds, t, Kindle.ACon (substVar (dict env) n) c)
  where cRhs env e [] []                = cBody env e
        cRhs env (ELam te e) ts es      = do (ds,te) <- cATEnv te
                                             let bs = mkBind te (zipWith3 match (rng te) ts es)
                                             (ds',t,c) <- cBody (addTEnv (Kindle.mkTEnv bs) env) e
                                             return (ds++ds', t, Kindle.CBind bs c)
        cRhs env e ts es                = do te <- newEnv paramSym ts
                                             let bs = mkBind te es
                                             (ds,t,c) <- cBody (addTEnv (Kindle.mkTEnv bs) env) e
                                             return (ds, t, Kindle.CBind bs c)

-- Convert a Core.Exp into a Kindle.AType and Kindle.Cmd
cBody env (ELet bs e)                   = do (ds,bs') <- cBindsList env (groupBinds bs)
                                             (ds',t,c) <- cBody (addTEnv (Kindle.mkTEnv bs') env) e
                                             return (ds++ds', t, Kindle.CBind bs' c)
cBody env (ECase e (a:alts) e')         = do (ds1,bs,t,e0) <- cExp env e
                                             (ds2,t1,a) <- cAlt cBody env e0 a
                                             (dss,alts) <- fmap unzip (mapM (cAltT cBodyT env t1 e0) alts)
                                             (ds3,c) <- cBodyT env t1 e'
                                             let ds = ds1 ++ ds2 ++ concat dss ++ ds3
                                             return (ds, t1, Kindle.cbind bs (Kindle.CSwitch (scrutinee env t e0) (a:alts) c))
cBody env (EAp (EVar (Prim Refl _) _) [e])      = cBody env e
cBody env (EAp (EVar (Prim Match _) _) [e])     = cBody env e
cBody env (EAp (EVar (Prim Commit _) _) [e])    = cBody env e
cBody env (EVar (Prim Fail _) _)                = return ([], Kindle.TWild, Kindle.CBreak)
cBody env (EAp (EVar (Prim Fatbar _) _) [e,e']) = do (ds,t,c) <- cBody env e
                                                     (ds',c') <- cBodyT env t e'
                                                     return (ds++ds', t, Kindle.CSeq c c')
cBody env e                             = do (ds,bs,t,e) <- cExp env e
                                             return (ds, t, Kindle.cbind bs (Kindle.CRet e))



-- Convert a Core.Exp into a Kindle.Exp, infer its Kindle.AType and overflow into a list of Kindle.Binds if necessary
cExp env e                              = do (ds,bs,t,h) <- cHead env e
                                             case h of
                                               VHead e -> 
                                                  return (ds, bs, t, e)
                                               FHead f ts -> do
                                                  te <- newEnv paramSym ts
                                                  x <- newName codeSym
                                                  n <- newName closureSym
                                                  let d = (n, Kindle.Struct [(x, Kindle.FunT ts t)])
                                                      b = (x, Kindle.Fun t te (Kindle.CRet (f (map Kindle.EVar (dom te)))))
                                                  return (d:ds, bs, Kindle.TId n, Kindle.ENew n [b])
                                               

data Head                               = VHead (Kindle.Exp)
                                        | FHead ([Kindle.Exp] -> Kindle.Exp) [Kindle.AType]


-- Convert a Core.Exp into an expression head that is either a value or a function
cHead env (ELit l)                      = return ([], [], Kindle.litType l, VHead (Kindle.ELit l))
cHead env (ERec c eqs)                  = do (ds,bs,bs') <- cEqs env te eqs
                                             return (ds, bs, Kindle.TId c, VHead (Kindle.ENew c bs'))
  where Kindle.Struct te                = lookup' (decls env) c
cHead env (ELet bs e)                   = do (ds,bs) <- cBindsList env (groupBinds bs)
                                             (ds',bs',t,h) <- cHead (addTEnv (Kindle.mkTEnv bs) env) e
                                             return (ds++ds', bs++bs', t, h)
cHead env (EAp (EVar (Prim Refl _) _) [e])       = cHead env e
cHead env (EAp (EVar (Prim ActToCmd _) _) [e])   = do (ds,bs,t,e) <- cExp env (EAp e defaults)
                                                      return (ds, bs, t, FHead (\_ -> e) [Kindle.TWild])
  where defaults                                 = [EAp (eVar (prim Sec)) [ELit (LInt 0)], eVar (prim Infinity)]
cHead env (EAp (EVar (Prim ReqToCmd _) _) [e])   = cHead env e
cHead env (EAp (EVar (Prim TemplToCmd _) _) [e]) = cHead env e
cHead env (EAp (EVar (Prim RefToPID _) _) [e])   = cHead env e
cHead env (EAp (EVar (Prim Match _) _) [e])      = cHead env e
cHead env (EAp (EVar (Prim Commit _) _) [e])     = cHead env e
cHead env (EAp (EVar (Prim After _) _) [e,e'])   = do (ds,bs,e1) <- cExpT env (Kindle.TId (prim Time)) e
                                                      (ds',bs',t,f,ts) <- cFHead env e'
                                                      return (ds++ds', bs++bs', t, FHead (\[a,b] -> f [sum a e1, b]) ts)
  where sum a e1                                 = Kindle.ECall (prim TimePlus) [a,e1]
cHead env (EAp (EVar (Prim Before _) _) [e,e'])  = do (ds,bs,e1) <- cExpT env (Kindle.TId (prim Time)) e
                                                      (ds',bs',t,f,ts) <- cFHead env e'
                                                      return (ds++ds', bs++bs', t, FHead (\[a,b] -> f [a, min b e1]) ts)
  where min b e1                                 = Kindle.ECall (prim TimeMin) [b,e1]
cHead env (EAp e es)                    = do (ds,bs,t,f,ts) <- cFHead env e
                                             (ds',bs',t',h) <- appFun t f ts es
                                             return (ds++ds', bs++bs', t', h)
  where appFun t f ts es
          | l_ts <  l_es                = error "Internal: c2k.appFun"
          | l_ts == l_es                = do (ds,bs,es) <- cExpTs env ts es
                                             return (ds, bs, t, VHead (f es))
          | l_ts >  l_es                = do (ds,bs,es) <- cExpTs env ts es
                                             return (ds, bs, t, FHead (\es' -> f (es++es')) ts2)
          where l_ts                    = length ts
                l_es                    = length es
                (ts1,ts2)               = splitAt (l_es) ts
cHead env (EVar x t')
  | stateVar (annot x)                  = case lookup' (tenv env) x of
                                             Kindle.FunT ts t -> error "Internal: state variable is a FHead in c2k.cHead"
                                             Kindle.ValT t    -> adjust t' [] [] t (VHead (Kindle.ESel (Kindle.EVar (self env)) x))
  | otherwise                           = case lookup' (tenv env) x of
                                             Kindle.FunT ts t -> adjust t' [] [] t (FHead (Kindle.ECall x) ts)
                                             Kindle.ValT t    -> adjust t' [] [] t (VHead (Kindle.EVar x))
cHead env (ESel e l t')                 = do (ds,bs,e) <- cExpT env t1 e
                                             case t2 of
                                               Kindle.FunT ts t -> adjust t' ds bs t (FHead (Kindle.EEnter e l) ts)
                                               Kindle.ValT t    -> adjust t' ds bs t (VHead (Kindle.ESel e l))
  where (t1,t2)                         = findSel env l
cHead env (ECon k t')                   = case t2 of
                                             Just te -> adjust t' [] [] t1 (FHead (Kindle.ECast t1 . Kindle.ENew k . mkBind te) (rng te))
                                             Nothing -> adjust t' [] [] t1 (VHead (Kindle.EVar k))
  where (t1,t2)                         = findCon env k
cHead env e                             = do (ds,te,t,c) <- cFun env e      -- Bottom-up mode!
                                             x <- newName tempSym
                                             let h = if null te then VHead (Kindle.ECall x []) else FHead (Kindle.ECall x) (rng te)
                                             return (ds, [(x, Kindle.Fun t te c)], t, h)


-- Adjust the type of a Head on bassis of the leaf identifier's annotated instance type (if any)
adjust Nothing ds bs t h                = return (ds, bs, t, h)
adjust (Just t0) ds bs t h              = do (ds',ts',t') <- cType (deQualify' t0)
                                             return (ds++ds', bs, t', adjust' t h ts' t')
  where adjust' t (VHead e) [] t'       = VHead (match t' t e)
        adjust' t (FHead f ts) ts' t'   = FHead (match t' t . f . zipWith3 match ts' ts) ts'


-- Convert a Core.Exp into an expression head that is expected to be a function
cFHead env e                            = do (ds,bs,Kindle.TId n,h) <- cHead env e
                                             case h of
                                                FHead f ts -> return (ds, bs, Kindle.TId n, f, ts)
                                                VHead e -> return (ds, bs, t, Kindle.EEnter e x, ts)
                                                  where Kindle.Struct te      = lookup' (ds ++ decls env) n
                                                        (x, Kindle.FunT ts t) = head te


-- Convert a Core.Cmd into a Kindle.AType and a Kindle.Cmd 
cCmd env (CRet e)                       = do (ds,bs,t,e) <- cExp env e
                                             return (ds, t, Kindle.cbind bs (Kindle.CRet e))
cCmd env (CAss x e c)                   = do (ds,bs,e) <- cExpT env tx e
                                             (ds',t,c) <- cCmd env c
                                             return (ds++ds', t, Kindle.cbind bs (Kindle.CAssign (Kindle.EVar (self env)) x e c))
  where tx                              = case lookup' (tenv env) x of
                                            Kindle.ValT t -> t
                                            _ -> error "Internal: c2k.cCmdT"
cCmd env (CLet bs c)                    = do (ds,bs') <- cBindsList env (groupBinds bs)
                                             (ds',t,c) <- cCmd (addTEnv (Kindle.mkTEnv bs') env) c
                                             return (ds++ds', t, Kindle.CBind bs' c)
cCmd env (CGen x tx e c)
  | isDummy x                           = do (ds1,bs,t,e) <- cExp env (EAp e [eVar (self env)])
                                             (ds2,t',c) <- cCmd env c
                                             return (ds1++ds2, t', Kindle.cbind bs (Kindle.CRun e c))
  | otherwise                           = do (ds1,tx) <- cAType tx
                                             (ds2,bs,e) <- cExpT env tx (EAp e [eVar (self env)])
                                             (ds3,t,c) <- cCmd (addTEnv [(x,Kindle.ValT tx)] env) c
                                             return (ds1++ds2++ds3, t, Kindle.cbind bs (Kindle.CBind [(x,Kindle.Val tx e)] c))
cCmd env (CExp e)                       = cCmdExp env e


-- Convert a Core.Exp in the monadic execution path into a Kindle.AType and a Kindle.Cmd
cCmdExp env (EDo x tx c)                = do (ds,tx) <- cAType tx
                                             (ds',t,c) <- cCmd (pushSelf x tx env) c
                                             return (ds++ds', t, Kindle.CBind [(x,Kindle.Val tx (Kindle.EVar (self env)))] c)
cCmdExp env (ECase e (a:alts) e')       = do (ds1,bs,t,e0) <- cExp env e
                                             (ds2,t1,a) <- cAlt cCmdExp env e0 a
                                             (dss,alts) <- fmap unzip (mapM (cAltT cCmdExpT env t1 e0) alts)
                                             (ds3,c) <- cBodyT env t1 e'
                                             let ds = ds1 ++ ds2 ++ concat dss ++ ds3
                                             return (ds, t1, Kindle.cbind bs (Kindle.CSwitch (scrutinee env t e0) (a:alts) c))
cCmdExp env e                           = do (ds,bs,t,e) <- cExp env (EAp e [eVar (self env)])
                                             return (ds, t, Kindle.cbind bs (Kindle.CRet e))
