module Core2Kindle(core2kindle) where
{- -}
import Monad
import Common
import Core
import Name
import Depend
import PP
import qualified Decls
import qualified Env
import qualified Kindle


-- =========================================================================================
-- Translation of Core modules into back-end Kindle format
-- =========================================================================================

core2kindle imp envI m              = do m <- localStore (cModule imp envI m)
                                         let Kindle.Module n ns ds bs = m
                                             te = map Kindle.mkSig bs
                                         return (m,(ds,te))

-- Note: bound variables in the output from this pass are no longer guaranteed to be globally unique!
-- This arises from the handling of orphaned state variables (see bindOrphans)


-- =========================================================================================
-- The FType format
-- =========================================================================================
    
-- This datatype is used to distinguish the types of known functions that can be called directly,
-- from those that are anonymous and thus must be translated as closures.  Furthermore, the type
-- arguments of FType constructors are supposed to be on a semi-Kindle form, where several primitive
-- type constructors have been replaced by their corresponding Kindle implementations.  These types
-- are still expressed using Core.Type syntax during the translation process, though, to facilitate
-- tracking of instantiation of type variables by means of unification.

data FType                          = FunT KEnv [Type] Type
                                    | ValT KEnv Type
                                    deriving (Eq,Show)

type FTEnv                          = Map Name FType


instance Subst FType Name Type where
    subst s (FunT ke ts t)          = FunT ke (subst s ts) (subst s t)
    subst s (ValT ke t)             = ValT ke (subst s t)
    

instance Subst FType TVar Type where
    subst s (FunT ke ts t)          = FunT ke (subst s ts) (subst s t)
    subst s (ValT ke t)             = ValT ke (subst s t)
    

-- =========================================================================================
-- Translation environment
-- =========================================================================================

data Env                            = Env { mname :: String,
                                            tenv  :: FTEnv,
                                            selfN :: Maybe Name }

env0 m                              = Env { mname = m,
                                            tenv  = [],
                                            selfN = Nothing }

addTEnv te env                      = env { tenv = te ++ tenv env }

pushAddSelf x t env                 = pushSelf x (addTEnv [(x,t)] env)

pushSelf x env                      = env { selfN = Just x }

haveSelf env                        = selfN env /= Nothing

self env                            = fromJust (selfN env)

nullSelf env                        = env { selfN = Nothing }


-- Look up a closure type (a Kindle.Struct) in the current store, extending the store if necessary
findClosureName env ts t            = do ds <- currentStore
                                         case find ds of
                                            Just n  -> return n
                                            Nothing -> do
                                               n <- newNameMod (Just (mname env)) closureSym
                                               addToStore (n, Kindle.Struct [(prim Code, Kindle.FunT ts t)] [])
                                               return n
  where find []                     = Nothing
        find ((n,Kindle.Struct [(x,Kindle.FunT ts' t')] _) : ds)  |  isClosure n && ts == ts' && t == t'     
                                    = Just n
        find (_ : ds)               = find ds


findClosureName' env (TFun ts t)    = do (t:ts) <- mapM (kindleType env) (t:ts)
                                         findClosureName env ts t
findClosureName' env _              = error "Internal: c2k.findClosureName'"


-- =========================================================================================
-- Translation entry point
-- =========================================================================================

-- Translate a Core.Module into a Kindle.Module
cModule ie (dsi,_,_) (Module m ns xs ds is bs)
                                    = do te <- tenv0 ie
                                         let env = addTEnv te (env0 (str m))
                                         ds1  <- cDecls env ds
                                         mapM_ addToStore (filter (isClosure . fst) dsi)
                                         bs  <- cBindsList env (groupBinds (is `catBinds` bs))
                                         ds2 <- currentStore
                                         return (Kindle.Module m ns (ds1++reverse (filter (isQual m . fst) ds2)) bs)


-- Compute the imported type environment
tenv0 (_,ds,bs,is)                  = cTEnv (tsigsOf is ++ tsigsOf bs ++ Decls.tenvSelsCons ds ++ Env.primTypeEnv)


-- =========================================================================================
-- Translating Core type declarations into Kindle.Decls
-- =========================================================================================

cDecls env (Types ke ds)            = do dss <- mapM cDecl ds
                                         return (concat dss)
  where cDecl (n,DRec _ vs [] ss)   = do te <- cTEnv ss
                                         te <- kindleTEnv env te
                                         return [(n, Kindle.Struct te [])]
        cDecl (n,DType vs t)        = return []
        cDecl (n,DData vs [] cs)    = do ds <- mapM (cCon te0) cs
                                         return ((n, Kindle.Struct te0 (dom cs)) : ds)
          where te0                 = [(prim Tag, Kindle.ValT (Kindle.TId (prim Int)))]

        cCon te0 (c,Constr ts ps _) = do te' <- cValTEnv te
                                         te' <- kindleTEnv env te'
                                         return (c, Kindle.Struct (te0++te') [])
          where te                  = abcSupply `zip` (ps++ts)


-- =========================================================================================
-- Translating schemes and types into Kindle-like (but still polymorphic) form
-- =========================================================================================

-- Translate a Core.TEnv into an FTEnv on basis of type arity
cTEnv te                                = do ts <- mapM (cScheme . snd) te
                                             return (dom te `zip` ts)
                                             

-- Translate a Core.TEnv into an FTEnv with only ValT types
cValTEnv te                             = do ts <- mapM (cValScheme . snd) te
                                             return (dom te `zip` ts)


-- Translate a Core.Scheme into an FType (ValT or FunT) on basis of type arity
cScheme sc                              = do (ke,ts,t) <- cType (deQualify sc)
                                             case ts of
                                                [] -> return (ValT (quant sc ++ ke) t)
                                                _  -> return (FunT (quant sc ++ ke) ts t)


-- Translate a Core.Scheme unconditionally into a ValT FType
cValScheme sc                           = do (ke,t) <- cAType (deQualify sc)
                                             return (ValT (quant sc ++ ke) t)
                                             

-- Translate a Core.Type nconditionally into a ValT FType
cValType t                              = do (ke,t) <- cAType t
                                             return (ValT ke t)


-- Peel off the quantifiers of a value type
stripValT (ValT _ t)                    = t
stripValT _                             = error "Internal: c2k.stripValT"
                                       

-- Reduce schemes and rho types to ordinary (function) types by simply removing the quantifiers, leaving the tyvars free.
deQualify (Scheme rh [] ke)             = deQualify' rh
deQualify (Scheme rh ps ke)             = case deQualify' rh of
                                            TFun ts t -> TFun (map deQualify ps ++ ts) t    -- concatenate ps with topmost params
                                            t         -> TFun (map deQualify ps) t
                                        

deQualify' (R t)                        = t
deQualify' (F ts rh)                    = TFun (map deQualify ts) (deQualify' rh)


-- Translate a Core.Type into a (possibly empty) list of atomic type parameters and an atomic return type
cType (TFun ts t)                       = do (kes,ts) <- fmap unzip (mapM cAType ts)
                                             (ke,ts',t) <- cType' t
                                             return (ke++concat kes, ts++ts', t)        -- concatenate ts with any monadic params
cType t                                 = cType' t


cType' (TId (Prim Action _))            = return ([], [t,t], TId (prim Msg))
  where t                               = TId (prim Time)
cType' (TAp (TId (Prim Request _)) t)
                                        = do (ke,t) <- cAType t
                                             v <- newName tyvarSym
                                             return ((v,Star):ke, [TId v], t)
cType' (TAp (TId (Prim Template _)) t)
                                        = do (ke,t) <- cAType t
                                             v <- newName tyvarSym
                                             return ((v,Star):ke, [TId v], t)
cType' (TAp (TAp (TId (Prim Cmd _)) s) t)
                                        = do (ke1,s) <- cAType s
                                             (ke2,t) <- cAType t
                                             return (ke1++ke2, [s], t)
cType' (TAp (TId (Prim Ref _)) t)       = do (ke,t) <- cAType t
                                             return (ke, [], t)
cType' (TAp (TId (Prim PMC _)) t)       = do (ke,t) <- cAType t
                                             return (ke, [], t)
cType' (TAp t1 t2)                      = do (ke1,t1) <- cAType t1
                                             (ke2,t2) <- cAType t2
                                             return (ke1++ke2, [], TAp t1 t2)
cType' t@(TId _)                        = return ([], [], t)
cType' t@(TVar _)                       = return ([], [], t)
cType' t                                = do (ke,t) <- cAType t
                                             return (ke, [], t)


-- Translate a Core.Type into an atomic type
cAType t                                = do (ke, ts, t) <- cType t
                                             case ts of
                                                [] -> return (ke, t)
                                                ts -> return (ke, TFun ts t)


-- =========================================================================================
-- Generating Kindle types
-- =========================================================================================

-- Convert an atomic Type into a Kindle.AType
kindleType env (TFun ts t)              = do (t':ts') <- mapM (kindleType env) (t:ts)
                                             n <- findClosureName env ts' t'
                                             return (Kindle.TId n)
kindleType env (TAp t _)                = kindleType env t
kindleType env (TId n)
  | isCon n                             = return (Kindle.TId n)
  | otherwise                           = return Kindle.TWild
kindleType env (TVar _)                 = return Kindle.TWild


kindleTEnv env te                       = mapM f te
  where f (x,FunT _ ts t)               = do (t:ts) <- mapM (kindleType env) (t:ts)
                                             return (x, Kindle.FunT ts t)
        f (x,ValT _ t)                  = do t <- kindleType env t
                                             return (x, Kindle.ValT t)
                                             

kindleATEnv env te                      = mapM f te
  where f (x,t)                         = do t <- kindleType env t
                                             return (x,t)
                                             

-- =========================================================================================
-- Adapting expected and inferred types
-- =========================================================================================

adaptFun env ts t te u c                = do (bf,te) <- adaptTE env ts te
                                             c <- Kindle.cMap f c
                                             return (te, bf c)
  where f e                             = liftM Kindle.CRet (adapt env t u e)


adaptTE env [] []                       = return (id, [])
adaptTE env (t:ts) ((x,u):te)           = do (bf,te) <- adaptTE env ts te
                                             y <- newName paramSym
                                             e <- adapt env u t (Kindle.EVar y)
                                             if e == Kindle.EVar y then
                                                 return (bf, (x,t):te)
                                              else do
                                                 u' <- kindleType env u
                                                 return (Kindle.cBind [(x,Kindle.Val u' e)] . bf, (y,t):te)


adaptExp env eqs t e                    = do e <- adapt env t1 t e
                                             return (t1, e)
  where t1                              = subst (quickUnify eqs) t
  

adapt env (TFun ts t) (TFun us u) e
  | or (zipWith mustBox (t:ts) (u:us))  = do (t':ts') <- mapM (kindleType env) (t:ts)
                                             te <- newEnv paramSym ts'
                                             es <- adaptL env us ts (map Kindle.EVar (dom te))
                                             e' <- adapt env t u (Kindle.EEnter e (prim Code) es)
                                             n <- findClosureName env ts' t'
                                             return (Kindle.ENew n [(prim Code, Kindle.Fun t' te (Kindle.CRet e'))])
adapt env t u e                         = do t' <- kindleType env t
                                             u' <- kindleType env u
                                             return (if t' == u' then e else Kindle.ECast t' e)


adaptL env [] [] []                     = return []
adaptL env (t:ts) (u:us) (e:es)         = do e <- adapt env t u e
                                             es <- adaptL env ts us es
                                             return (e:es)
                                             

mustBox (TId (Prim p _)) (TVar _)       = p `elem` nonPointers
mustBox (TVar _) (TId (Prim p _))       = p `elem` nonPointers
mustBox _ _                             = False


-- =========================================================================================
-- Translating bindings
-- =========================================================================================

-- Translate a list of strongly connected Core binding groups into a list of Kindle bindings
cBindsList env []                       = return []
cBindsList env (bs:bss)                 = do (te,bf) <- cBinds env bs
                                             bs <- cBindsList (addTEnv te env) bss
                                             return (flat (bf Kindle.CBreak) ++ bs)
  where flat (Kindle.CBind r bs c)      = bs ++ flat c                    -- temporary, awaiting full recursive value implementation
        flat _                          = []


-- Translate a list of (mutually recursive) Core bindings into a list of Kindle bindings on basis of declared type
cBinds env (Binds rec te eqs)           = do te <- cTEnv te
                                             assert (not rec || all okRec (rng te)) "Illegal value recursion"
                                             (bf,_,bs) <- cEqs (if rec then addTEnv te env else env) te eqs
                                             -- (Ignore returned type equalities, because te will have no unification variables)
                                             return (te, bf . Kindle.CBind rec bs)
  where okRec (ValT ke t)               = okRec' t
        okRec (FunT ke ts t)            = True                            -- Good: recursive function
        okRec' (TFun _ _)               = True                            -- ( won't appear, but good anyway )
        okRec' (TId (Prim p _))         = p `notElem` nonPointers         -- Bad: recursive non pointer (can't use placeholder)
        okRec' (TId n)                  = isCon n                         -- Bad: recursive type variable (black hole)
        okRec' (TVar _)                 = False                           -- ( won't appear, but bad anyway )
        okRec' (TAp t _)                = okRec' t                        -- Discard type arguments


-- Translate a list of Core equations into a list of Kindle bindings on basis of declared type
cEqs env te eqs                         = do (bfs,eqs,bs) <- fmap unzip3 (mapM cEq eqs)
                                             return (foldr (.) id bfs, eqs, bs)
  where cEq (x,e)                       = case lookup' te x of
                                            ValT _ t -> do                       -- don't instantiate; leave skolemized instead!
                                                (bf,eq,e) <- cExpT env t e
                                                t <- kindleType env t
                                                return (bf, eq, (x, Kindle.Val t e))
                                            FunT _ ts t -> do                    -- don't instantiate; leave skolemized instead!
                                                (te,eq,c) <- cFunT (nullSelf env) ts t e
                                                t <- kindleType env t
                                                te <- kindleATEnv env te
                                                return (id, eq, (x, Kindle.Fun t te c))


-- Translate a Core.Exp with a known Type into a Kindle.Exp
cExpT env t e                           = do (bf,t0,e) <- cExp env e
                                             e <- adapt env t t0 e
                                             return (bf, (t0,t), e)
                                             
cExpTs env [] []                        = return (id, [], [])
cExpTs env (t:ts) (e:es)                = do (bf,eq,e) <- cExpT env t e
                                             (bf',eqs,es) <- cExpTs env ts es
                                             return (bf . bf', eq:eqs, e:es)


-- Translate a Core.Exp with a known function type into a Kindle.Cmd and an argument list
cFunT env ts t e                        = do (te,u,c) <- cFun env e
                                             (te',c') <- absFun te u c
                                             return (te', (TFun ts t, TFun (rng te) u), c')
  where absFun te u c
          | l_ts >  l_te                = do (te,c) <- adaptFun env ts1 (TFun ts2 t) te u c
                                             xs <- newNames paramSym (length ts2)
                                             c <- Kindle.cMap (f (map Kindle.EVar xs)) c
                                             return (te++(xs`zip`ts2), c)
          | l_ts == l_te                = adaptFun env ts t te u c
          | l_ts <  l_te                = do (te1,c) <- adaptFun env ts t te1 (TFun us u) c
                                             (u:us) <- mapM (kindleType env) (u:us)
                                             n <- findClosureName env us u
                                             return (te1, Kindle.CRet (Kindle.ENew n [(prim Code, Kindle.Fun u (xs`zip`us) c)]))
          where l_ts                    = length ts
                l_te                    = length te
                (ts1,ts2)               = splitAt l_te ts
                (te1,te2)               = splitAt l_ts te
                f es e                  = return (Kindle.CRet (Kindle.EEnter e (prim Code) es))
                (xs,us)                 = unzip te2


-- =========================================================================================
-- Translating abstractions
-- =========================================================================================

-- Convert a Core.Exp into a parameter list and a Kindle.Cmd, inferring its parameter and return Types
cFun env (ELam te e)                    = do te <- cValTEnv te
                                             (te',t,c) <- cFun (addTEnv te env) e
                                             return (mapSnd stripValT te ++ te', t, c)
cFun env (EReq (EVar x) e)              = do (t,c) <- cCmdExp (pushSelf x env) e
                                             t' <- kindleType env t
                                             c <- Kindle.protect x t' c
                                             y <- newName dummySym
                                             u <- newTVar Star
                                             return ([(y,u)], t, c)
cFun env (EReq e e')                    = do (bf,tx,e) <- cExp env e
                                             x <- newName selfSym
                                             (t,c) <- cCmdExp (pushAddSelf x (ValT [] tx) env) e'
                                             t' <- kindleType env t
                                             c <- Kindle.protect x t' c
                                             y <- newName dummySym
                                             u <- newTVar Star
                                             tx' <- kindleType env tx
                                             return ([(y,u)], t, bf (Kindle.cBind [(x,Kindle.Val tx' e)] c))
cFun env e@(EAct _ _)                   = cAct env id id e
cFun env e@(EAp (EVar (Prim After _)) _) 
                                        = cAct env id id e
cFun env e@(EAp (EVar (Prim Before _)) _) 
                                        = cAct env id id e
cFun env (ETempl x tx te c)             = do tx <- cValType tx     -- Type-checker guarantees tx is a struct type name
                                             tx'@(Kindle.TId n) <- kindleType env (stripValT tx)
                                             te <- cValTEnv te
                                             (t,c) <- cCmd (pushAddSelf x tx (addTEnv te env)) c
                                             te' <- kindleTEnv env te
                                             addToStore (n, Kindle.Struct te' [])
                                             y <- newName dummySym
                                             u <- newTVar Star
                                             return ([(y,u)], t, Kindle.cBind [(x,Kindle.Val tx' (Kindle.ENew n []))] c)
cFun env (EDo x tx c)                   = do tx <- cValType tx
                                             (t,c) <- cCmd (pushAddSelf x tx env) c
                                             return ([(x,stripValT tx)], t, c)
cFun env e                              = do (t,c) <- cBody env e
                                             return ([], t, c)


-- Convert an action expression into a Core.Cmd (agnostic mode)
cAct env fa fb (EAp (EVar (Prim After _)) [e,e'])
                                        = do (bf,_,e1) <- cExpT env tTime e
                                             -- ignore resulting type equalities, no unification variables passed in via tTime
                                             (te,t,c) <- cAct env (sum e1 . fa) fb e'
                                             return (te, t, bf c)
  where sum e1 a                        = Kindle.ECall (prim TimePlus) [e1,a]
cAct env fa fb (EAp (EVar (Prim Before _)) [e,e'])
                                        = do (bf,_,e1) <- cExpT env tTime e
                                             -- ignore resulting type equalities, no unification variables passed in via tTime
                                             (te,t,c) <- cAct env fa (min e1 . fb) e'
                                             return (te, t, bf c)
  where min e1 b                        = Kindle.ECall (prim TimeMin) [e1,b]
cAct env fa fb (EAct e e')              = do (_,_,c) <- cFun env (EReq e e')
                                             -- Ignore returned te (must be unused) and result type (will be replaced below)
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             m  <- newName tempSym
                                             let c'  = Kindle.cBind bs (Kindle.CRun e1 (Kindle.CRet (Kindle.EVar m)))
                                                 bs  = [(m, Kindle.Val ktMsg (Kindle.ENew (prim Msg) bs'))]
                                                 bs' = [(prim Code, Kindle.Fun (Kindle.TId (prim UNITTYPE)) [] c)]
                                                 es  = [Kindle.EVar m, fa (Kindle.EVar a), fb (Kindle.EVar b)]
                                                 e1  = Kindle.ECall (prim ASYNC) es
                                             c' <- Kindle.cMap (\_ -> return (Kindle.CRet (Kindle.EVar (prim UNITTERM)))) c'
                                             return ([(a,tTime),(b,tTime)], tMsg, c')
cAct env fa fb e                        = do (bf,t0,f,_,[ta,tb]) <- cFHead env e
                                             -- ignore resulting type equalities, no unification variables to instantiate in a tMsg
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             let c = bf (Kindle.CRet (f [fa (Kindle.EVar a), fb (Kindle.EVar b)]))
                                             return ([(a,tTime), (b,tTime)], tMsg, c)

tTime                                   = TId (prim Time)
tMsg                                    = TId (prim Msg)
ktMsg                                   = Kindle.TId (prim Msg)


-- =========================================================================================
-- Translating let- and case expressions
-- =========================================================================================

-- Convert a Core (Pat,Exp) pair into a Kindle.Alt, inferring the result type.  Use e0 to find constructor fields.
cAlt cBody env e0 (PLit l, e)           = do (t,c) <- cBody env e
                                             return (t, Kindle.ALit l c)
cAlt cBody env e0 (PCon k, e)           = do (ts,_) <- instCon env k
                                             -- ignore result type and don't unify with scrutinee type; no need to compute the 
                                             -- instantiated field types since e' will contain typed binders for each field anyway.
                                             (t,c) <- cRhs cBody env e ts (map (Kindle.ESel e1) (take (length ts) abcSupply))
                                             return (t, Kindle.ACon k c)
  where e1                              = Kindle.ECast (Kindle.TId k) e0


-- Convert a Core right-hand-side into a Kindle.AType and a Kindle.Cmd (with bindings)
cRhs cBody env e [] []                  = cBody env e
cRhs cBody env (ELam te e) ts es        = do te <- cValTEnv te
                                             es' <- adaptL env (map (stripValT . snd) te) ts1 es1
                                             -- (Skolemize te, types are *upper* bounds)
                                             (t,c) <- cRhs cBody (addTEnv te env) e ts2 es2
                                             (xs,ts') <- fmap unzip (kindleATEnv env (mapSnd stripValT te))
                                             return (t, Kindle.cBind (mkBinds xs ts' es') c)
          where (es1,es2)               = splitAt (length te) es
                (ts1,ts2)               = splitAt (length te) ts
cRhs cBody env e ts es                  = do te <- newEnv paramSym ts
                                             (t,c) <- cBody (addTEnv (mapSnd (ValT []) te) env) (EAp e (map EVar (dom te)))
                                             (xs,ts') <- fmap unzip (kindleATEnv env te)
                                             return (t, Kindle.cBind (mkBinds xs ts' es) c)


-- Map a Kindle.ATEnv (unzipped) and a list of Kindle.Exps into a list of Kindle.Binds
mkBinds xs ts es                        = zipWith3 f xs ts es
  where f x t e                         = (x, Kindle.Val t e)


-- Generate the operations necessary to acces the tag of a case head expression
scrutinee (TAp t _)                     = scrutinee t
scrutinee (TId (Prim p _))
  | p `elem` nonPointers                = id
scrutinee _                             = \e -> Kindle.ESel e (prim Tag)


-- Convert a Core.Exp into a Kindle.AType and Kindle.Cmd
cBody env (ELet bs e)                   = do (te,bf) <- cBinds env bs
                                             (t,c) <- cBody (addTEnv te env) e
                                             return (t, bf c)
cBody env (ECase e ((PCon k,e'):_) _)
  | isTuple k                           = do (bf,_,e0) <- cExp env e
                                             (ts,_) <- instCon env k
                                             -- ignore result type and don't unify with scrutinee type; no need to compute the 
                                             -- instantiated field types since e' will contain typed binders for each field anyway.
                                             (t,c) <- cRhs cBody env e' ts (map (Kindle.ESel e0) (take (length ts) abcSupply))
                                             return (t, bf c)
cBody env (ECase e alts e')             = do (bf,t0,e0) <- cExp env e
                                             (ts,alts) <- fmap unzip (mapM (cAlt cBody env e0) alts)
                                             (t,c) <- cBody env e'
                                             let s = quickUnify (repeat t `zip` ts)
                                             return (subst s t, bf (Kindle.CSwitch (scrutinee t0 e0) alts c))
cBody env (EAp (EVar (Prim Refl _)) [e])      = cBody env e
cBody env (EAp (EVar (Prim Match _)) [e])     = cBody env e
cBody env (EAp (EVar (Prim Commit _)) [e])    = cBody env e
cBody env (EVar (Prim Fail _))                = do t <- newTVar Star
                                                   return (t, Kindle.CBreak)
cBody env (EAp (EVar (Prim Fatbar _)) [e,e']) = do (t,c) <- cBody env e
                                                   (t',c') <- cBody env e'
                                                   return (subst (quickUnify [(t,t')]) t, Kindle.CSeq c c')
cBody env e                             = do (bf,t,e) <- cExp env e
                                             return (t, bf (Kindle.CRet e))


-- =========================================================================================
-- Translating commands
-- =========================================================================================

-- Convert a Core.Cmd into a Kindle.Cmd, inferring a Type
cCmd env (CRet e)                       = do (bf,t,e) <- cExp env e
                                             bindOrphans env t bf e (Kindle.CRet e)
cCmd env (CAss x e c)                   = do (bf,_,e) <- cExpT env (stripValT tx) e
                                             -- Skolemize tx, type is an *upper* bound
                                             -- Ignore returned type equalities, no unification variables in tx
                                             (t,c) <- cCmd env c
                                             bindOrphans env t bf e (Kindle.CAssign (Kindle.EVar (self env)) x e c)
  where tx                              = lookup' (tenv env) x
cCmd env (CLet bs c)                    = do (te,bf) <- cBinds env bs
                                             (t,c) <- cCmd (addTEnv te env) c
                                             bindOrphans' env t bf c
cCmd env (CGen x tx e c)
  | isDummy x                           = do (bf,t,e) <- cExp env (EAp e [EVar (self env)])
                                             (t',c) <- cCmd env c
                                             bindOrphans env t' bf e (Kindle.CRun e c)
  | otherwise                           = do tx <- cValType tx  
                                             (bf,_,e) <- cExpT env (stripValT tx) (EAp e [EVar (self env)])
                                             -- Skolemize tx, type is an *upper* bound
                                             -- Ignore returned type equalities, no unification variables in tx
                                             (t,c) <- cCmd (addTEnv [(x,tx)] env) c
                                             tx <- kindleType env (stripValT tx)
                                             bindOrphans env t bf e (Kindle.cBind [(x,Kindle.Val tx e)] c)
cCmd env (CExp e)                       = cCmdExp env e


-- Convert a Core.Exp in the monadic execution path into a Kindle.Cmd, inferring a Type
cCmdExp env (EDo x tx c)                = do tx <- cValType tx
                                             (t,c) <- cCmd (pushAddSelf x tx env) c
                                             tx <- kindleType env (stripValT tx)
                                             return (t, Kindle.cBind [(x,Kindle.Val tx (Kindle.EVar (self env)))] c)
cCmdExp env (ECase e ((PCon k,e'):_) _)
  | isTuple k                           = do (bf,_,e0) <- cExp env e
                                             (ts,_) <- instCon env k
                                             (t,c) <- cRhs cCmdExp env e' ts (map (Kindle.ESel e0) (take (length ts) abcSupply))
                                             bindOrphans env t bf e0 c
cCmdExp env (ECase e alts e')           = do (bf,t0,e0) <- cExp env e
                                             (ts,alts) <- fmap unzip (mapM (cAlt cCmdExp env e0) alts)
                                             (t,c) <- cBody env e'
                                             let s = quickUnify (repeat t `zip` ts)
                                             bindOrphans env (subst s t) bf e0 (Kindle.CSwitch (scrutinee t0 e0) alts c)
cCmdExp env e                           = do (bf,t,e) <- cExp env (EAp e [EVar (self env)])
                                             bindOrphans env t bf e (Kindle.CRet e)


-- State variables are normally translated into field selections from the current "self".  For example,
--     x := 7; return (x + 1)
-- gets translated into
--     self->x := 7; return (self->x + 1)
-- However, closure values, which may be invoked long after they are defined, must not be sensitive to state
-- mutations.  This means that "self" dereferencing operations for any contained state variables must be done 
-- when the closure is defined, not when it is invoked.  Here's a challenging example:
--     x := 7; f = \y->y+x; x := 2; return (f 1);
-- If we naively translate the x reference in f to self->x, we end up with the wrong behavior:
--     self->x := 77; f(y) { return self->x + y }; self->x := 7; return f(1);        (3 is returned instead of 8)
-- The correct translation is instead to dereference x in the state where f is defined:
--     self->x := 7; int x = self->x; f(y) { return x+y }; self->x := 2; return f(1);
-- State variables like x above will be left as they are when translated inside function closures, a context 
-- identified by the lack of a current "self" (c.f. cHead (EVar x)).  What remains to be done then is to 
-- insert bindings for any such "orphaned" variables before each command, and this is exactly what "bindOrphans" 
-- does.  It takes an expression e in which to look for orphans (together with binding overflow bf), and 
-- prepends the resulting self dereferencing bindings to the given command c.  For convenience, the result is 
-- also paired with a supplied result type t0.
bindOrphans env t0 bf e c
  | not (haveSelf env)                  = return (t0, bf c)
  | otherwise                           = do ts <- mapM mkKindleType vs
                                             return (t0, Kindle.cBind (zipWith mkSelBind vs ts) (bf c))
  where vs                              = nub (filter isState (idents e ++ idents (bf Kindle.CBreak)))
        mkSelBind v t                   = (v, Kindle.Val t (Kindle.ESel (Kindle.EVar (self env)) v))
        mkKindleType v                  = kindleType env t
          where ValT ke t               = lookup' (tenv env) v
 
bindOrphans' env t0 bf c                = bindOrphans env t0 bf (Kindle.EVar (prim UNITTERM)) c



-- =========================================================================================
-- Translating atomic expressions
-- =========================================================================================

-- Translate a Core.Exp into a Kindle.Exp, inferring its Type and overflowing into a list of Kindle.Binds if necessary
cExp env e                              = do (bf,t,h) <- cHead env e
                                             case h of
                                               VHead e -> 
                                                  return (bf, t, e)
                                               FHead f eqs ts -> do
                                                  xs <- newNames paramSym (length ts)
                                                  es <- adaptL env ts ts1 (map Kindle.EVar xs)
                                                  e1 <- adapt env t1 t (f es)
                                                  (t':ts') <- mapM (kindleType env) (t1:ts1)
                                                  n <- findClosureName env ts' t'
                                                  let fun = Kindle.Fun t' (xs `zip` ts') (Kindle.CRet e1)
                                                  return (bf, TFun ts1 t1, Kindle.ENew n [(prim Code, fun)])
                                                 where s   = quickUnify eqs
                                                       t1  = subst s t
                                                       ts1 = subst s ts


data Head                               = VHead (Kindle.Exp)
                                        | FHead ([Kindle.Exp] -> Kindle.Exp) [(Type,Type)] [Type]


-- Translate a Core.Exp into an expression Head that is either a value or a function
cHead env (ELit l)                      = return (id, litType l, VHead (Kindle.ELit l))
cHead env (ERec c eqs)                  = do ((t0:ts0),ts1) <- fmap unzip (mapM (instSel env) ls)
                                             let s = quickUnify (repeat t0 `zip` ts0)
                                                 te = ls `zip` subst s ts1
                                             (bf,equalities,bs) <- cEqs env te eqs
                                             return (bf, subst (quickUnify equalities) t0, VHead (Kindle.ENew c bs))
  where (ls,es)                         = unzip eqs
cHead env (ELet bs e)                   = do (te,bf) <- cBinds env bs
                                             (bf',t,h) <- cHead (addTEnv te env) e
                                             return (bf . bf', t, h)
cHead env (EAp (EVar (Prim Refl _)) [e])       = cHead env e
cHead env (EAp (EVar (Prim ActToCmd _)) [e])   = do (bf,t,e) <- cExp env (EAp e [EVar (prim Inherit), EVar (prim Inherit)])
                                                    t' <- newTVar Star
                                                    return (bf, t, FHead (\_ -> e) [] [t'])
cHead env (EAp (EVar (Prim ReqToCmd _)) [e])   = cHead env e
cHead env (EAp (EVar (Prim TemplToCmd _)) [e]) = cHead env e
cHead env (EAp (EVar (Prim RefToPID _)) [e])   = cHead env e
cHead env (EAp (EVar (Prim Match _)) [e])      = cHead env e
cHead env (EAp (EVar (Prim Commit _)) [e])     = cHead env e
cHead env (EAp (EVar (Prim After _)) [e,e'])   = do (bf,_,e1) <- cExpT env tTime e
                                                    (bf',t,f,_,ts) <- cFHead env e'
                                                    return (bf . bf', t, FHead (\[a,b] -> f [sum a e1, b]) [] ts)
  where sum a e1                               = Kindle.ECall (prim TimePlus) [a,e1]
cHead env (EAp (EVar (Prim Before _)) [e,e'])  = do (bf,_,e1) <- cExpT env tTime e
                                                    (bf',t,f,_,ts) <- cFHead env e'
                                                    return (bf . bf', t, FHead (\[a,b] -> f [a, min b e1]) [] ts)
  where min b e1                               = Kindle.ECall (prim TimeMin) [b,e1]
cHead env (EAp e es)                    = do (bf,t,f,eqs,ts) <- cFHead env e
                                             appFun env bf t f eqs ts es
  where appFun env bf t f eqs0 ts es
          | l_ts <  l_es                = do (bf',eqs,es1) <- cExpTs env ts es1
                                             (TFun ts' t',e') <- adaptExp env (eqs0++eqs) t (f es1)
                                             appFun env (bf . bf') t' (Kindle.EEnter e' (prim Code)) [] ts' es2
          | l_ts == l_es                = do (bf',eqs,es) <- cExpTs env ts es
                                             (t',e') <- adaptExp env (eqs0++eqs) t (f es)
                                             return (bf . bf', t', VHead e')
          | l_ts >  l_es                = do (bf',eqs,es) <- cExpTs env ts1 es
                                             return (bf . bf', t, FHead (f . (es++)) (eqs0++eqs) ts2)
          where l_ts                    = length ts
                l_es                    = length es
                (ts1,ts2)               = splitAt l_es ts
                (es1,es2)               = splitAt l_ts es
cHead env (EVar x)                      = do (ts,t) <- instT (lookup' (tenv env) x)
                                             case ts of
                                               [] -> if stateVar (annot x) && haveSelf env 
                                                     then return (id, t, VHead (Kindle.ESel (Kindle.EVar (self env)) x))
                                                     else return (id, t, VHead (Kindle.EVar x))
                                               _  ->      return (id, t, FHead (Kindle.ECall x) [] ts)
cHead env (ESel e l)                    = do (t0,t1) <- instSel env l
                                             (ts,t) <- instT t1
                                             (bf,eq,e) <- cExpT env t0 e
                                             case ts of
                                               [] -> do (t',e') <- adaptExp env [eq] t (Kindle.ESel e l)
                                                        return (bf, t', VHead e')
                                               _  -> return (bf, t, FHead (Kindle.EEnter e l) [eq] ts)
cHead env (ECon k)                      = do (ts,t1) <- instCon env k
                                             u <- kindleType env t1
                                             let tagB = ((prim Tag, Kindle.Val (Kindle.TId (prim Int)) (Kindle.EVar k)):)
                                                 newK = if isTuple k then Kindle.ENew k else Kindle.ECast u . Kindle.ENew k . tagB
                                             case ts of
                                               [] -> return (id, t1, VHead (newK []))
                                               _  -> do ts' <- mapM (kindleType env) ts
                                                        return (id, t1, FHead (newK . mkBinds abcSupply ts') [] ts)
cHead env e@(ECase _ _ _)               = do (t,c) <- cBody env e       -- Bottom-up mode!
                                             x <- newName tempSym
                                             t' <- kindleType env t
                                             return (Kindle.cBind [(x, Kindle.Fun t' [] c)], t, VHead (Kindle.ECall x []))
cHead env e                             = do (te,t,c) <- cFun (nullSelf env) e     -- Bottom-up mode!
                                             x <- newName tempSym
                                             t' <- kindleType env t
                                             te' <- kindleATEnv env te
                                             return (Kindle.cBind [(x, Kindle.Fun t' te' c)], t, FHead (Kindle.ECall x) [] (rng te))


-- Translate a Core.Exp into an expression head that is expected to be a function
cFHead env e                            = do (bf,t,h) <- cHead env e
                                             case h of
                                                FHead f eqs ts -> return (bf, t, f, eqs, ts)
                                                VHead e -> return (bf, t', Kindle.EEnter e (prim Code), [], ts)
                                                  where TFun ts t' = t


-- =========================================================================================
-- Unification and instantiation
-- =========================================================================================

-- Unification
quickUnify []                           = nullSubst
quickUnify ((TVar n,TVar n'):eqs)
  | n == n'                             = quickUnify eqs
quickUnify ((TVar n, t):eqs)            = quickUnify (subst s eqs) @@ s
  where s                               = n +-> t
quickUnify ((t,TVar n):eqs)             = quickUnify (subst s eqs) @@ s
  where s                               = n +-> t
quickUnify ((TAp t t',TAp u u'):eqs)    = quickUnify ((t,u) : (t',u') : eqs)
quickUnify ((TFun ts t,TFun us u):eqs)
  | length ts == length us              = quickUnify ((t,u) : (ts `zip` us) ++ eqs)
  | otherwise                           = error "Internal: c2k.quickUnify"
quickUnify ((t,u):eqs)                  = quickUnify eqs


-- Instantiation
instT (FunT ke ts t)                    = do s <- instKE ke
                                             return (subst s ts, subst s t)
instT (ValT ke t)                       = do s <- instKE ke
                                             return ([], subst s t)


instKE ke                               = do ts <- mapM newTVar ks
                                             return (vs `zip` ts)
  where (vs,ks)                         = unzip ke


instSel env l                           = do s <- instKE ke0
                                             return (subst s t0, subst s (f t1))
  where FunT ke [t0] t1                 = lookup' (tenv env) l
        (ke0,ke1)                       = partition ((`elem` tyvars t0) . fst) ke
        f (TFun ts t)                   = FunT ke1 ts t
        f t                             = ValT ke1 t


instCon env (Tuple n _)                 = cScheme (tupleType n) >>= instT
instCon env k                           = instT (lookup' (tenv env) k)
