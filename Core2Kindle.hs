module Core2Kindle(core2kindle, c2kTEnv) where
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

core2kindle e2 e3 m                 = localStore (cModule e2 e3 m)


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


data Result a                       = ValR a
                                    | FunR ([Kindle.Exp] -> a) [(Type,Type)] [Type]

rcomp f (ValR c)                    = ValR (f c)
rcomp f (FunR g eqs ts)             = FunR (f . g) eqs ts


instance Subst FType Name Type where
    subst s (FunT ke ts t)          = FunT ke (subst s ts) (subst s t)
    subst s (ValT ke t)             = ValT ke (subst s t)
    

instance Subst FType TVar Type where
    subst s (FunT ke ts t)          = FunT ke (subst s ts) (subst s t)
    subst s (ValT ke t)             = ValT ke (subst s t)
    

-- =========================================================================================
-- Translation environment
-- =========================================================================================

data Env                            = Env { mname   :: String,
                                            tenv    :: FTEnv,
                                            selfN   :: Maybe Name }


env0 m                              = Env { mname   = m,
                                            tenv    = [],
                                            selfN   = Nothing }

addTEnv te env                      = env { tenv = te ++ tenv env }

pushAddSelf x t env                 = pushSelf x (addTEnv [(x,t)] env)

pushSelf x env                      = env { selfN = Just x }

self env                            = fromJust (selfN env)


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
findClosureName' env _              = internalError0 "c2k.findClosureName'"


-- =========================================================================================
-- Translation entry point
-- =========================================================================================

-- Translate a Core.Module into a Kindle.Module
cModule e2 e3 (Module m ns xs ds is bs)
                                    = do te0 <- tenv0 e2
                                         te  <- cTEnv (Decls.tenvSelsCons ds)
                                         let env = addTEnv (te ++ te0) (env0 (str m))
                                         ds1  <- cDecls env ds
                                         mapM_ addToStore (filter (isClosure . fst) e3)
                                         bs  <- cBindsList env (groupBinds (is `catBinds` bs))
                                         ds2 <- currentStore
                                         let ds3 = ds1++reverse (filter (isQual m . fst) ds2)
                                         return (Kindle.Module m ns ds3 bs,ds3)


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
                                             

-- Translate a Core.Type unconditionally into a ValT FType
cValType t                              = do (ke,t) <- cAType t
                                             return (ValT ke t)


-- Peel off the quantifiers of a value type
stripQuant (ValT _ t)                   = t
stripQuant (FunT _ ts t)                = TFun ts t
                                       

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

-- Adapt a Kindle function (parameter list and body) to the required parameter and result types.
adaptFun env ts t [] (TFun us u) c      = do xs <- newNames paramSym (length us)
                                             adaptFun env ts t (xs `zip` us) u (Kindle.cmap (Kindle.enter2 (map Kindle.EVar xs)) c)
adaptFun env ts t ue u c
  | l_ts >  l_ue                        = do (ue,c) <- adaptFun env ts1 (TFun ts2 t) ue u c
                                             xs <- newNames paramSym (length ts2)
                                             return (ue++(xs`zip`ts2), Kindle.cmap (Kindle.enter2 (map Kindle.EVar xs)) c)
  | l_ts <  l_ue                        = do (ue1,c) <- adaptFun env ts t ue1 (TFun us u) c
                                             (u:us) <- mapM (kindleType env) (u:us)
                                             n <- findClosureName env us u
                                             return (ue1, Kindle.CRet (Kindle.new n u (xs`zip`us) c))
  where l_ts                            = length ts
        l_ue                            = length ue
        (ts1,ts2)                       = splitAt l_ue ts
        (ue1,ue2)                       = splitAt l_ts ue
        (xs,us)                         = unzip ue2
adaptFun env ts t ue u c                = do (bf,ue) <- adaptTE ts ue
                                             f <- adapt env t u
                                             return (ue, bf (Kindle.cmap f c))
  where adaptTE [] []                   = return (id, [])
        adaptTE (t:ts) ((x,u):ue)       = do (bf,ue) <- adaptTE ts ue
                                             f <- adapt env u t
                                             if f Kindle.EThis == Kindle.EThis then
                                                 return (bf, (x,t):ue)
                                              else do
                                                 u' <- kindleType env u
                                                 y <- newName paramSym
                                                 return (Kindle.cBind [(x,Kindle.Val u' (f (Kindle.EVar y)))] . bf, (y,t):ue)


-- Adapt a Kindle expression of given type to the type obtained by unifiying a list of type equations
adaptExp env eqs t e                    = do f <- adapt env t1 t
                                             return (t1, f e)
  where t1                              = subst (quickUnify eqs) t
  

-- Adapt an unknown Kindle expression of a given type to a required type
adapt env (TFun ts t) (TFun us u)
  | len_ts > len_us                     = do f <- adapt env (TFun ts1 (TFun ts2 t)) (TFun us u)
                                             (t':ts') <- mapM (kindleType env) (t:ts)
                                             te <- newEnv paramSym ts'
                                             n <- findClosureName env ts' t'
                                             let (es1,es2) = splitAt len_us (map Kindle.EVar (dom te))
                                                 ent e = Kindle.enter (Kindle.enter e es1) es2
                                             return (\e -> Kindle.new n t' te (Kindle.CRet (ent (f e))))
  | len_ts < len_us                     = do f <- adapt env t (TFun us2 u)
                                             g <- adaptL env us1 ts
                                             (t':ts') <- mapM (kindleType env) (t:ts)
                                             (u':us') <- mapM (kindleType env) (u:us2)
                                             (xs1,xs2) <- fmap (splitAt len_ts) (newNames paramSym (len_us))
                                             n1 <- findClosureName env ts' t'
                                             n2 <- findClosureName env us' u'
                                             let --exp1 e = Kindle.ENew n1 [(prim Code, Kindle.Fun t' (xs1 `zip` ts') (Kindle.CRet (f e)))]
                                                 --exp2 c = Kindle.ENew n2 [(prim Code, Kindle.Fun u' (xs2 `zip` us') c)]
                                                 cmd e  = Kindle.CRet (Kindle.enter e (g (map Kindle.EVar xs1) ++ map Kindle.EVar xs2))
                                                 exp1 e = Kindle.new n1 t' (xs1 `zip` ts') (Kindle.CRet (f e))
                                                 exp2 c = Kindle.new n2 u' (xs2 `zip` us') c
                                             return (exp1 . exp2 . cmd)
  where len_ts                          = length ts
        len_us                          = length us
        (ts1,ts2)                       = splitAt len_us ts
        (us1,us2)                       = splitAt len_ts us
adapt env (TFun ts t) (TFun us u)
  | explicit (t:ts) (u:us)              = do (t':ts') <- mapM (kindleType env) (t:ts)
                                             te <- newEnv paramSym ts'
                                             f <- adapt env t u
                                             f' <- adaptL env us ts
                                             n <- findClosureName env ts' t'
                                             let ent e = f (Kindle.enter e (f' (map Kindle.EVar (dom te))))
                                             return (\e -> Kindle.new n t' te (Kindle.CRet (ent e)))
  where explicit ts us                  = or (zipWith expl ts us)
        expl (TId (Prim p _)) (TVar _)  = p `elem` Kindle.boxedPrims
        expl (TVar _) (TId (Prim p _))  = p `elem` Kindle.boxedPrims
        expl (TFun ts t) (TFun us u)    = length ts /= length us || explicit (t:ts) (u:us)
        expl _ _                        = False
adapt env t u                           = do t' <- kindleType env t
                                             u' <- kindleType env u
                                             return (if t' == u' then id else Kindle.ECast t')


-- As above, but generalized to lists
adaptL env ts us                        = do fs <- mapM (uncurry (adapt env)) (ts `zip` us)
                                             return (\es -> zipWith ($) fs es)


-- Adapt a typed result according to the type obtained by applying a substitution
adaptR env (_,t0) (t, ValR c)           = do g <- adapt env t0 t
                                             return (t0, ValR (Kindle.cmap g c))
adaptR env ([n],t0) (t, FunR f eqs ts)  = do g <- adapt env t1 t
                                             g' <- adaptL env ts ts1
                                             return (t1, FunR (Kindle.cmap g . f . g') (eqs'++eqs) ts1)
  where TFun ts1 t1                     = t0
        eqs'                            = (t1,t) : ts1 `zip` ts
adaptR env j (t, FunR f eqs ts)         = do xs <- newNames paramSym (length ts)
                                             (t':ts') <- mapM (kindleType env) (t:ts)
                                             n <- findClosureName env ts' t'
                                             let close c = Kindle.CRet (Kindle.new n t' (xs `zip` ts') c)
                                             adaptR env j (TFun ts t, ValR (Kindle.clift close (f (map Kindle.EVar xs))))
        

joinR rs                                = (nub arities, subst (quickUnify (repeat t `zip` ts)) t)
  where (arities, t:ts)                 = unzip rs


rtype (t, ValR _)                       = (0, t)
rtype (t, FunR f eqs ts)                = (length ts, TFun ts t)




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
                                             assert (not rec || all okRec (rng te)) "Illegal value recursion" (dom te)
                                             (bf,_,bs) <- cEqs (if rec then addTEnv te env else env) te eqs
                                             -- (Ignore returned type equalities, because te will have no unification variables)
                                             return (te, bf . Kindle.CBind rec bs)
  where okRec (ValT ke t)               = okRec' t
        okRec (FunT ke ts t)            = True                            -- Good: recursive function
        okRec' (TFun _ _)               = True                            -- ( won't appear, but good anyway )
        okRec' (TId (Prim p _))         = p `notElem` Kindle.boxedPrims   -- Bad: type that can't fit placeholder
        okRec' (TId n)                  = isCon n                         -- Bad: type variable (black hole)
        okRec' (TVar _)                 = False                           -- ( won't appear, but bad anyway )
        okRec' (TAp t _)                = okRec' t                        -- Discard type arguments


-- Translate a list of Core equations into a list of Kindle bindings on basis of declared type
cEqs env te eqs                         = do (bfs,eqs,bs) <- fmap unzip3 (mapM cEq eqs)
                                             return (foldr (.) id bfs, eqs, bs)
  where cEq (x,e)                       = case lookup' te x of
                                            ValT _ t -> do                       -- don't instantiate; leave skolemized instead!
                                                (bf,eq,e) <- cValExpT env t e
                                                t <- kindleType env t
                                                return (bf, eq, (x, Kindle.Val t e))
                                            FunT ke ts t -> do                    -- don't instantiate; leave skolemized instead!
                                                (te,eq,c) <- cFunT env ts t e
                                                t <- kindleType env t
                                                te <- kindleATEnv env te
                                                return (id, eq, (x, Kindle.Fun t te c))


-- Translate a Core.Exp with a known Type into a Kindle.Exp
cValExpT env t e                        = do (bf,t0,e') <- cValExp env e
                                             f <- adapt env t t0
                                             return (bf, (t0,t), f e')
                                             
cValExpTs env [] []                     = return (id, [], [])
cValExpTs env (t:ts) (e:es)             = do (bf,eq,e) <- cValExpT env t e
                                             (bf',eqs,es) <- cValExpTs env ts es
                                             return (bf . bf', eq:eqs, e:es)


-- Translate a Core.Exp with a known function type into a Kindle.Cmd and an argument list
cFunT env ts t e                        = do (te,u,c) <- cFun env e
                                             (te',c') <- adaptFun env ts t te u c
                                             return (te', (TFun ts t, TFun (rng te) u), c')


-- =========================================================================================
-- Translating abstractions
-- =========================================================================================

-- Convert a Core.Exp into a parameter list and a Kindle.Cmd, inferring its parameter and return Types
cFun env (ELam te e)                    = do te <- cValTEnv te
                                             (te',t,c) <- cFun (addTEnv te env) e
                                             return (mapSnd stripQuant te ++ te', t, c)
cFun env (EReq e e')                    = do (bf,tx,e) <- cValExp env e
                                             x <- newName selfSym
                                             (t,c) <- cCmdExp (pushAddSelf x (ValT [] tx) env) e'
                                             y <- newName dummySym
                                             u <- newTVar Star
                                             tx' <- kindleType env tx
                                             let bf' = Kindle.cBind [(x,Kindle.Val tx' (Kindle.lock tx' e))] . Kindle.unlock x
                                             return ([(y,u)], t, bf (bf' c))
cFun env e@(EAct _ _)                   = cAct env id id e
cFun env e@(EAp (EVar (Prim After _)) _) 
                                        = cAct env id id e
cFun env e@(EAp (EVar (Prim Before _)) _) 
                                        = cAct env id id e
cFun env (ETempl x tx te c)             = do tx <- cValType tx     -- Type-checker guarantees tx is a struct type name
                                             tx'@(Kindle.TId n) <- kindleType env (stripQuant tx)
                                             te <- cValTEnv te
                                             (t,c) <- cCmd (pushAddSelf x tx (addTEnv te env)) c
                                             te' <- kindleTEnv env te
                                             addToStore (n, Kindle.Struct (Kindle.objTEnv ++ te') [])
                                             y <- newName dummySym
                                             u <- newTVar Star
                                             let e = Kindle.ENew n Kindle.objInit
                                             return ([(y,u)], t, Kindle.cBind [(x,Kindle.Val tx' e)] c)
cFun env (EDo x tx c)                   = do tx <- cValType tx
                                             (t,c) <- cCmd (pushAddSelf x tx env) c
                                             return ([(x,stripQuant tx)], t, c)
cFun env e                              = do (t,r) <- cBody env e
                                             case r of
                                               FunR f eqs ts -> do xs <- newNames paramSym (length ts)
                                                                   g <- adapt env t1 t
                                                                   g' <- adaptL env ts ts1
                                                                   let c = f (g' (map Kindle.EVar xs))
                                                                   return (xs `zip` ts1, t1, Kindle.cmap g c)
                                                 where s      = quickUnify eqs
                                                       t1     = subst s t
                                                       ts1    = subst s ts
                                               ValR c        -> return ([], t, c)


-- Translate an action expression into a Core.Cmd, inferring its type
cAct env fa fb (EAp (EVar (Prim After _)) [e,e'])
                                        = do (bf,_,e1) <- cValExpT env tTime e
                                             -- ignore resulting type equalities, no unification variables passed in via tTime
                                             (te,t,c) <- cAct env (sum e1 . fa) fb e'
                                             return (te, t, bf c)
  where sum e1 a                        = Kindle.ECall (prim TimePlus) [e1,a]
cAct env fa fb (EAp (EVar (Prim Before _)) [e,e'])
                                        = do (bf,_,e1) <- cValExpT env tTime e
                                             -- ignore resulting type equalities, no unification variables passed in via tTime
                                             (te,t,c) <- cAct env fa (min e1 . fb) e'
                                             return (te, t, bf c)
  where min e1 b                        = Kindle.ECall (prim TimeMin) [e1,b]
cAct env fa fb (EAct e e')              = do (_,_,c) <- cFun env (EReq e e')
                                             -- Ignore returned te (must be unused) and result type (will be replaced below)
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             m  <- newName tempSym
                                             let c1  = Kindle.cBind bs (Kindle.CRun e1 (Kindle.CRet (Kindle.EVar m)))
                                                 c2  = Kindle.cmap (\_ -> Kindle.EVar (prim UNITTERM)) c
                                                 bs  = [(m, Kindle.Val ktMsg (Kindle.ENew (prim Msg) bs'))]
                                                 bs' = [(prim Code, Kindle.Fun (Kindle.TId (prim UNITTYPE)) [] c2)]
                                                 es  = [Kindle.EVar m, fa (Kindle.EVar a), fb (Kindle.EVar b)]
                                                 e1  = Kindle.ECall (prim ASYNC) es
                                             return ([(a,tTime),(b,tTime)], tMsg, c1)
cAct env fa fb e                        = do (bf,t0,f,_,[ta,tb]) <- cFunExp env e
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

-- Translate a Core (Pat,Exp) pair into a Kindle.Alt, inferring the result type.  Use e0 to find constructor fields.
cAlt cBdy env e0 t0 (PLit l, e)         = do (t1,c) <- cBdy env e
                                             return (litType l, (t1, rcomp (Kindle.ALit l) c))
cAlt cBdy env e0 t0 (PCon k, e)         = do (ts,t) <- instCon env k
                                             -- no need to unify t with t0 and compute the instantiated field types,
                                             -- since e will contain typed binders for each field anyway.
                                             f <- adapt env t t0
                                             let e1 = Kindle.ECast (Kindle.TId k) (f e0)
                                             (t1,c) <- cRhs cBdy env e ts (map (Kindle.ESel e1) (take (length ts) abcSupply))
                                             return (t, (t1, rcomp (Kindle.ACon k) c))
cAlt cBdy env e0 t0 (PWild, e)          = do (t1,c) <- cBdy env e
                                             t <- newTVar Star
                                             return (t, (t1, rcomp Kindle.AWild c))


-- Translate a Core right-hand-side into a Kindle.Cmd (with bindings), inferring its type
cRhs cBdy env e [] []                   = cBdy env e
cRhs cBdy env (ELam te e) ts es         = do te <- cValTEnv te
                                             f <- adaptL env (map (stripQuant . snd) te) ts1
                                             -- (Skolemize te, types are *upper* bounds)
                                             (t,c) <- cRhs cBdy (addTEnv te env) e ts2 es2
                                             (xs,ts') <- fmap unzip (kindleATEnv env (mapSnd stripQuant te))
                                             return (t, rcomp (Kindle.cBind (mkBinds xs ts' (f es1))) c)
          where (es1,es2)               = splitAt (length te) es
                (ts1,ts2)               = splitAt (length te) ts
cRhs cBdy env e ts es                   = do te <- newEnv paramSym ts
                                             (t,c) <- cBdy (addTEnv (mapSnd (ValT []) te) env) (EAp e (map EVar (dom te)))
                                             (xs,ts') <- fmap unzip (kindleATEnv env te)
                                             return (t, rcomp (Kindle.cBind (mkBinds xs ts' es)) c)


-- Map a Kindle.ATEnv (unzipped) and a list of Kindle.Exps into a list of Kindle.Binds
mkBinds xs ts es                        = zipWith3 f xs ts es
  where f x t e                         = (x, Kindle.Val t e)


-- Generate the operations necessary to acces the tag of a case head expression
scrutinee (TAp t _)                     = scrutinee t
scrutinee (TId (Prim p _))
  | p `elem` [Int, Float, Time, Char]   = id
scrutinee _                             = \e -> Kindle.ESel e (prim Tag)


isMatch (EVar (Prim Match _))           = True
isMatch _                               = False


-- Translate a Core.Exp into a Kindle.Cmd that returns a function or a value, inferring result type and possible parameters
cBody env (ELet bs e)                   = do (te,bf) <- cBinds env bs
                                             (t,r) <- cBody (addTEnv te env) e
                                             return (t, rcomp bf r)
cBody env (ECase e alts)                = cCase cBody env e alts
cBody env (EAp m [e]) | isMatch m       = do (t,r) <- cPMC cBody env e
                                             return (t, rcomp (\c -> Kindle.CSeq c (Kindle.CRaise (Kindle.ELit (LInt Nothing 1)))) r)
cBody env e                             = do (bf,t,h) <- cExp env e
                                             case h of
                                               ValR e        -> return (t, ValR (bf (Kindle.CRet e)))
                                               FunR f eqs ts -> return (t, FunR (bf . Kindle.CRet . f) eqs ts)


-- Note: we don't really handle PMC terms as first class citizens, rather like constructors in a small grammar
-- of pattern-matching expressions:
-- e  ::=  ...  |  Match pm
-- pm ::=  Commit e  |  Fail  |  Fatbar pm pm  |  case e of {p -> pm} pm  |  let bs in pm
-- This syntax is followed when PMC terms are introduced in module Match, and is also respected by Termred.
--
-- However, should we for some reason want to allow abstraction over PMC terms, as in (\e -> Commit e),
-- the translation below will need to be complemented with a concrete implementation of the PMC type constructor
-- (using Maybe, for example), and corresponding general implementations of Match, Commit, Fail & Fatbar.


-- Translate a Core.Exp corresponding to a PMC term into a Kindle.Cmd result, and infer its result type
cPMC cE env (ELet bs e)                 = do (te,bf) <- cBinds env bs
                                             (t,c) <- cPMC cE (addTEnv te env) e
                                             return (t, rcomp bf c)
cPMC cE env (ECase e alts)              = cCase (cPMC cE) env e alts
cPMC cE env (EAp (EVar (Prim Fatbar _)) [e1,e2])
                                        = do r1 <- cPMC cE env e1
                                             r2 <- cPMC cE env e2
                                             let j = joinR [rtype r1, rtype r2]
                                             r1' <- adaptR env j r1
                                             r2' <- adaptR env j r2
                                             return (mkSeq r1' r2')
cPMC cE env (EVar (Prim Fail _))        = do t <- newTVar Star
                                             return (t, ValR Kindle.CBreak)
cPMC cE env (EAp (EVar (Prim Commit _)) [e])
                                        = cE env e
cPMC cE env e                           = internalError "PMC syntax violated in Core2Kindle" e


-- Translate the parts of a case expression into a Kindle.Cmd result, and infer its result type
cCase cE env e ((PCon k,e'):_)
  | isTuple k                           = do (bf,t0,e0) <- cValExp env e
                                             (ts,t) <- instCon env k
                                             -- no need to unify t with t0 and compute the instantiated field types,
                                             -- since e' will contain typed binders for each field anyway.
                                             f <- adapt env t t0
                                             (t1,r) <- cRhs cE env e' ts (map (Kindle.ESel (f e0)) (take (length ts) abcSupply))
                                             return (t1, rcomp bf r)
cCase cE env e alts                     = do (bf,t0,e0) <- cValExp env e
                                             (ts0,rs) <- fmap unzip (mapM (cAlt cE env e0 t0) alts)
                                             let j = joinR (map rtype rs)
                                             rs' <- mapM (adaptR env j) rs
                                             f <- adapt env (head ts0) t0
                                             return (mkSwitch bf rs' (scrutinee (head ts0) (f e0)))


mkSwitch bf ((t,ValR c):rs) e0          = (t, ValR (bf (Kindle.CSwitch e0 alts)))
  where alts                            = c : [ alt | (_,ValR alt) <- rs ]     
mkSwitch bf ((t,FunR g eqs ts):rs) e0   = (t, FunR (\es -> bf (Kindle.CSwitch e0 (map ($es) (g:gs)))) (eqs'++eqs) ts)
  where gs                              = [ g | (_,FunR g _ _) <- rs ]
        eqs'                            = concat [ eqs' | (_,FunR _ eqs' _) <- rs ]

mkSeq (t1,ValR c1) (t2,ValR c2)         = (t1, ValR (Kindle.CSeq c1 c2))
mkSeq (t1,FunR g1 eqs1 ts1) (t2,FunR g2 eqs2 ts2)
                                        = (t1, FunR (\es -> Kindle.CSeq (g1 es) (g2 es)) (eqs1++eqs2) ts1)


-- =========================================================================================
-- Translating commands
-- =========================================================================================

-- Convert a Core.Cmd into a Kindle.Cmd, inferring a Type
cCmd env (CRet e)                       = do (bf,te,e) <- freezeState env e
                                             (bf',t,e) <- cValExp (addTEnv te env) e
                                             if Kindle.simpleExp e then         -- No state references or non-termination in e
                                                 return (t, bf (bf' (Kindle.CRet e)))   -- Can be ignored (see CGen alternative)
                                              else do
                                                 x <- newName tempSym
                                                 t' <- kindleType env t
                                                 let bf'' = Kindle.cBind [(x, Kindle.Val t' e)]
                                                 return (t, bf (bf' (bf'' (Kindle.CRet (Kindle.EVar x)))))
cCmd env (CAss x e c)                   = do (bf,te,e) <- freezeState env e
                                             (bf',_,e) <- cValExpT (addTEnv te env) (stripQuant tx) e
                                             -- Skolemize tx, type is an *upper* bound
                                             -- Ignore returned type equalities, no unification variables in tx
                                             (t,c) <- cCmd env c
                                             return (t, bf (bf' (Kindle.CUpdS (Kindle.EVar (self env)) x e c)))
  where tx                              = lookup' (tenv env) x
cCmd env (CLet bs c)                    = do (bf,te,bs) <- freezeState env bs
                                             (te',bf') <- cBinds (addTEnv te env) bs
                                             (t,c) <- cCmd (addTEnv te' env) c
                                             return (t, bf (bf' c))
cCmd env (CGen x tx (ECase e alts) c)
  | isDummy x                           = do (_,ValR c1) <- cCase cValCmdExp env e (filter useful alts)
                                             (t,c2) <- cCmd env c
                                             return (t, Kindle.CSeq (Kindle.cMap (\_ -> Kindle.CBreak) c1) c2)
  where useful (_,EDo _ _ (CRet (ECon (Prim UNITTERM _))))  = False
        useful _                                            = True
cCmd env (CGen x tx e c)                = do (bf,te,e) <- freezeState env e
                                             tx <- cValType tx
                                             (bf',_,e) <- cValExpT (addTEnv te env) (stripQuant tx) (EAp e [EVar (self env)])
                                             -- Skolemize tx, type is an *upper* bound
                                             -- Ignore returned type equalities, no unification variables in tx
                                             (t,c) <- cCmd (addTEnv [(x,tx)] env) c
                                             tx <- kindleType env (stripQuant tx)
                                             return (t, bf (bf' (Kindle.cBind [(x,Kindle.Val tx e)] c)))
cCmd env (CExp e)                       = cCmdExp env e


-- Convert a Core.Exp in the monadic execution path into a Kindle.Cmd, inferring a Type
cCmdExp env (EAp (EVar (Prim ReqToCmd _)) [e])  
                                        = cCmdExp env e
cCmdExp env (EAp (EVar (Prim TemplToCmd _)) [e])
                                        = cCmdExp env e
cCmdExp env (EDo x tx c)                = do tx <- cValType tx
                                             (t,c) <- cCmd (pushAddSelf x tx env) c
                                             tx <- kindleType env (stripQuant tx)
                                             return (t, Kindle.cBind [(x,Kindle.Val tx (Kindle.EVar (self env)))] c)
cCmdExp env (ELet bs e)                 = do (bf,te,bs) <- freezeState env bs
                                             (te',bf') <- cBinds (addTEnv te env) bs
                                             (t,c) <- cCmdExp (addTEnv te' env) e
                                             return (t, bf (bf' c))
cCmdExp env (ECase e alts)              = do (bf,te,e) <- freezeState env e
                                             (t,ValR c) <- cCase cValCmdExp (addTEnv te env) e alts
                                             return (t, bf c)
cCmdExp env (EAp m [e]) | isMatch m     = do (bf,te,e) <- freezeState env e
                                             (t,ValR c) <- cPMC cValCmdExp (addTEnv te env) e
                                             return (t, bf (Kindle.CSeq c (Kindle.CRaise (Kindle.ELit (LInt Nothing 1)))))
cCmdExp env e                           = do (bf,te,e) <- freezeState env e
                                             (bf',t,e) <- cValExp (addTEnv te env) (EAp e [EVar (self env)])
                                             x <- newName tempSym
                                             t' <- kindleType env t
                                             let bf'' = Kindle.cBind [(x, Kindle.Val t' e)]
                                             return (t, bf (bf' (bf'' (Kindle.CRet (Kindle.EVar x)))))


cValCmdExp env e                        = do (t,c) <- cCmdExp env e
                                             return (t, ValR c)
                                             

-- State variables are normally translated into field selections from the current "self".  For example,
--     x := 7; return (x + 1)
-- gets translated into
--     self->x := 7; return (self->x + 1)
-- However, closure values, which may be invoked long after they are defined, must not be sensitive to state
-- mutations.  This means that "self" dereferencing operations for any contained state variables must be done 
-- when the closure is defined, not when it is invoked.  Here's a challenging example:
--     x := 7; f = \y->x+y; x := 2; return (f 1);
-- If we naively translate the x reference in f to self->x, we end up with the wrong behavior:
--     self->x := 7; int f(int y) { return self->x + y }; self->x := 2; return f(1);    -- gives 3, not 8
--
-- A correct translation can instead be obtained if we make sure that no state variables are ever referenced 
-- from within a lambda abstraction, as in the equivalent example
--     x := 7; x' = x; f = \y->x'+y; x := 2; return (f 1);                      (for some fresh variable x')
-- Achieving this is the job of function freezeState below.  It takes an expression e and returns a renaming 
-- of e with the "fragile" free variables (i.e., state vars inside lambdas) replaced by fresh variables, 
-- together with a type environment and Kindle bindings for the new variables.  For the example above, 
-- input (\y->x+y) results in output (\y->x'+y) together with the Kindle binding (int x' = self->x).  When
-- composed with the rest of the translation process, the full output becomes
--     self->x := 7; int x' = self->x; int f(int y) { return x'+y }; self->x := 2; return f(1);
-- This form is perfectly valid as input to the subsequent lambda-lifting pass, whose result will be
--     int f(int x'', int y) { return x'' + y }
--     self->x := 7; int x' = self->x; self->x := 2; return f(x',1);


class Fragile a where
    fragile :: a -> [Name]
    
instance Fragile Exp where
    fragile (ELam _ e)                  = filter isState (idents e)
    fragile (EAp e es)                  = concatMap fragile (e:es)
    fragile (ESel e l)                  = fragile e
    fragile (ERec c eqs)                = concatMap fragile (rng eqs)
    fragile (ELet bs e)                 = fragile bs ++ fragile e
    fragile (ECase e alts)              = fragile e ++ concatMap fragile alts
    fragile _                           = []

instance Fragile (Pat,Exp) where
    fragile (_,ELam te e)               = fragile e
    fragile (_,e)                       = fragile e
    
instance Fragile Binds where
    fragile bs                          = concatMap fragile (rng (eqnsOf bs))


freezeState :: (Fragile a, Subst a Name Exp) => Env -> a -> M (Name,Kindle.Decl) (Kindle.Cmd->Kindle.Cmd, FTEnv, a)
-- Why is this signature necessary????
freezeState env e
  | null vs                             = return (id, [], e)
  | otherwise                           = do vs' <- newNames paramSym (length vs)
                                             (bf,_,bs) <- cEqs env (vs' `zip` ts) (vs' `zip` sels)
                                             return (bf . Kindle.cBind bs, vs' `zip` ts, subst (vs `zip` map EVar vs') e)
  where vs                              = nub (fragile e)
        ts                              = map (lookup' (tenv env)) vs
        sels                            = map (ESel (EVar (self env))) vs
        

-- =========================================================================================
-- Translating expressions
-- =========================================================================================

-- Translate a Core.Exp into an expression result that is either a value or a function
cExp env (ELit l)                       = return (id, litType l, ValR (Kindle.ELit l))
cExp env (ERec c eqs)                   = do (t0:ts0,ts1) <- fmap unzip (mapM (instSel env) ls)
                                             let s = quickUnify (repeat t0 `zip` ts0)
                                                 te = ls `zip` subst s ts1
                                             (bf,equalities,bs) <- cEqs env te eqs
                                             return (bf, subst (quickUnify equalities) t0, ValR (Kindle.ENew c bs))
  where (ls,es)                         = unzip eqs
cExp env (ELet bs e)                    = do (te,bf) <- cBinds env bs
                                             (bf',t,h) <- cExp (addTEnv te env) e
                                             return (bf . bf', t, h)
cExp env (EAp (EVar (Prim ActToCmd _)) [e])    = do (bf,t,e) <- cValExp env (EAp e [EVar (prim Inherit), EVar (prim Inherit)])
                                                    t' <- newTVar Star
                                                    return (bf, t, FunR (\_ -> e) [] [t'])
cExp env (EAp (EVar (Prim ReqToCmd _)) [e])    = cExp env e
cExp env (EAp (EVar (Prim TemplToCmd _)) [e])  = cExp env e
cExp env (EAp (EVar (Prim RefToPID _)) [e])    = cExp env e
cExp env (EAp (EVar (Prim After _)) [e,e'])    = do (bf,_,e1) <- cValExpT env tTime e
                                                    (bf',t,f,_,ts) <- cFunExp env e'
                                                    return (bf . bf', t, FunR (\[a,b] -> f [sum a e1, b]) [] ts)
  where sum a e1                               = Kindle.ECall (prim TimePlus) [a,e1]
cExp env (EAp (EVar (Prim Before _)) [e,e'])   = do (bf,_,e1) <- cValExpT env tTime e
                                                    (bf',t,f,_,ts) <- cFunExp env e'
                                                    return (bf . bf', t, FunR (\[a,b] -> f [a, min b e1]) [] ts)
  where min b e1                               = Kindle.ECall (prim TimeMin) [b,e1]
cExp env (EAp e es) | not (isMatch e)   = do (bf,t,f,eqs,ts) <- cFunExp env e
                                             appFun env bf t f eqs ts es
  where appFun env bf t f eqs0 ts es
          | l_ts <  l_es                = do (bf',eqs,es1) <- cValExpTs env ts es1
                                             (TFun ts' t',e') <- adaptExp env (eqs0++eqs) t (f es1)
                                             appFun env (bf . bf') t' (Kindle.enter e') [] ts' es2
          | l_ts == l_es                = do (bf',eqs,es) <- cValExpTs env ts es
                                             (t',e') <- adaptExp env (eqs0++eqs) t (f es)
                                             return (bf . bf', t', ValR e')
          | l_ts >  l_es                = do (bf',eqs,es) <- cValExpTs env ts1 es
                                             return (bf . bf', t, FunR (f . (es++)) (eqs0++eqs) ts2)
          where l_ts                    = length ts
                l_es                    = length es
                (ts1,ts2)               = splitAt l_es ts
                (es1,es2)               = splitAt l_ts es
cExp env (EVar x)                       = do (ts,t) <- instT (lookup' (tenv env) x)
                                             case ts of
                                               [] -> return (id, t, ValR e)
                                               _  -> return (id, t, FunR (Kindle.ECall x) [] ts)
  where e                               = if stateVar (annot x) then Kindle.ESel (Kindle.EVar (self env)) x else Kindle.EVar x
cExp env (ESel e l)                     = do (t0,t1) <- instSel env l
                                             (ts,t) <- instT t1
                                             (bf,eq,e) <- cValExpT env t0 e
                                             case ts of
                                               [] -> do (t',e') <- adaptExp env [eq] t (Kindle.ESel e l)
                                                        return (bf, t', ValR e')
                                               _  -> return (bf, t, FunR (Kindle.EEnter e l) [eq] ts)
cExp env (ECon k)                       = do (ts,t1) <- instCon env k
                                             u <- kindleType env t1
                                             let tagB = ((prim Tag, Kindle.Val (Kindle.TId (prim Int)) (Kindle.EVar k)):)
                                                 newK = if isTuple k then Kindle.ENew k else Kindle.ECast u . Kindle.ENew k . tagB
                                             case ts of
                                               [] -> return (id, t1, ValR (newK []))
                                               _  -> do ts' <- mapM (kindleType env) ts
                                                        return (id, t1, FunR (newK . mkBinds abcSupply ts') [] ts)
cExp env e                              = do (te,t,c) <- cFun env e
                                             t' <- kindleType env t
                                             case te of
                                               [] -> do x <- newName tempSym
                                                        return (Kindle.cBind [(x, Kindle.Fun t' [] c)], t, ValR (Kindle.ECall x []))
                                               _  -> do te' <- kindleATEnv env te
                                                        n <- findClosureName env (rng te') t'
                                                        return (id, TFun (rng te) t, ValR (Kindle.new n t' te' c))


-- Translate a Core.Exp into a Kindle value expression, inferring its Type and overflowing into a list of Kindle.Binds if necessary
cValExp env e                           = do (bf,t,h) <- cExp env e
                                             case h of
                                               ValR e -> 
                                                  return (bf, t, e)
                                               FunR f eqs ts -> do
                                                  xs <- newNames paramSym (length ts)
                                                  g <- adapt env t1 t
                                                  g' <- adaptL env ts ts1
                                                  (t':ts') <- mapM (kindleType env) (t1:ts1)
                                                  n <- findClosureName env ts' t'
                                                  let es = map Kindle.EVar xs
                                                      te = xs `zip` ts'
                                                      c = Kindle.cmap g (Kindle.CRet (f (g' es)))
                                                  return (bf, TFun ts1 t1, Kindle.new n t' te c)
                                                 where s   = quickUnify eqs
                                                       t1  = subst s t
                                                       ts1 = subst s ts


-- Translate a Core.Exp into a Kindle application head, inferring its Type and overflowing into a list of Kindle.Binds if necessary
cFunExp env e                           = do (bf,t,h) <- cExp env e
                                             case h of
                                                FunR f eqs ts -> return (bf, t, f, eqs, ts)
                                                ValR e -> return (bf, t', Kindle.enter e, [], ts)
                                                  where TFun ts t' = t


-- =========================================================================================
-- Unification and instantiation
-- =========================================================================================

-- Unification
quickUnify []                           = nullSubst
quickUnify ((TFun (t:ts) t',TFun (u:us) u'):eqs)
                                        = quickUnify ((t,u) : (TFun ts t', TFun us u') : eqs)
quickUnify ((TFun [] t, u):eqs)         = quickUnify ((t,u) : eqs)
quickUnify ((t, TFun [] u):eqs)         = quickUnify ((t,u) : eqs)
quickUnify ((TVar n,TVar n'):eqs)
  | n == n'                             = quickUnify eqs
quickUnify ((TVar n, t):eqs)            = quickUnify (subst s eqs) @@ s
  where s                               = n +-> t
quickUnify ((t,TVar n):eqs)             = quickUnify (subst s eqs) @@ s
  where s                               = n +-> t
quickUnify ((TAp t t',TAp u u'):eqs)    = quickUnify ((t,u) : (t',u') : eqs)
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
                                             return (subst s t0, subst s (f ts0 t1))
  where FunT ke (t0:ts0) t1             = lookup' (tenv env) l
        (ke0,ke1)                       = partition ((`elem` tyvars t0) . fst) ke
        f [] (TFun ts t)                = FunT ke1 ts t
        f [] t                          = ValT ke1 t
        f ts t                          = FunT ke1 ts t


instCon env (Tuple n _)                 = cScheme (tupleType n) >>= instT
instCon env k                           = instT (lookup' (tenv env) k)

c2kTEnv ds te                           = localStore f
  where f                               = do mapM_ addToStore (filter (isClosure . fst) ds)
                                             te <- cTEnv te
                                             te <- kindleTEnv (env0 "") te
                                             return (filter p te)
        p (_,Kindle.FunT _ (Kindle.TId n))    
                                        = not(isClosure n)
        p _                             = True
