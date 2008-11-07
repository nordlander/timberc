module Core2Kindle(core2kindle, c2kTEnv) where
{- -}
import Monad
import Common
import Core
import Name
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

data Result a                       = ValR a
                                    | FunR ([Kindle.Exp] -> a) [Kindle.AType]

rcomp f (ValR c)                    = ValR (f c)
rcomp f (FunR g ts)                 = FunR (f . g) ts


-- =========================================================================================
-- Translation environment
-- =========================================================================================

data Env                            = Env { mname   :: Maybe String,
                                            decls   :: Kindle.Decls,
                                            tenv    :: Kindle.TEnv,
                                            selfN   :: Maybe Name,
                                            tArgs   :: [Kindle.AType] }


env0                                = Env { mname   = Nothing,
                                            decls   = [],
                                            tenv    = [],
                                            selfN   = Nothing,
                                            tArgs   = [] }

setMName m env                      = env { mname = Just (str m) }

addDecls ds env                     = env { decls = ds ++ decls env }

addTEnv te env                      = env { tenv = te ++ tenv env }

addATEnv te env                     = env { tenv = mapSnd Kindle.ValT te ++ tenv env }

pushSelf x env                      = env { selfN = Just x }

self env                            = fromJust (selfN env)

setTArgs env ts                     = env { tArgs = ts }

findDecl env k
  | isTuple k                       = Kindle.tupleDecl k
  | otherwise                       = lookup' (decls env) k
  

-- Look up a closure type (a Kindle.Struct) in the current store, extending the store if necessary
findClosureType env [] ts t         = do ds <- currentStore
                                         -- tr ("Looking for (1) " ++ render (pr (name0 "closure", Kindle.FunT [] ts t)))
                                         case Kindle.findStruct s0 ds of
                                            Just n  -> return (Kindle.TCon n (t:ts))
                                            Nothing -> do
                                               n <- newNameMod (mname env) (closureSym ++ show a)
                                               addToStore (n, s0)
                                               return (Kindle.TCon n (t:ts))
  where s0                          = Kindle.Struct vs0 [(prim Code, Kindle.FunT [] (tail ts0) (head ts0))] Kindle.Top
        vs0                         = take (a + 1) abcSupply
        ts0                         = map Kindle.tVar vs0
        a                           = length ts
findClosureType env vs ts t         = do ds <- currentStore
                                         -- tr ("Looking for (2) " ++ render (pr (name0 "closure", Kindle.FunT vs ts t)))
                                         case Kindle.findStruct s0 ds of
                                            Just n  -> return (Kindle.TCon n ts0)
                                            Nothing -> do
                                               n <- newNameMod (mname env) closureSym
                                               addToStore (n, s0)
                                               return (Kindle.TCon n ts0)
  where s0                          = Kindle.Struct vs0 [(prim Code, Kindle.FunT vs ts t)] Kindle.Top
        vs0                         = nub (Kindle.typevars (t:ts) \\ vs)
        ts0                         = map Kindle.tVar vs0


openClosureType (Kindle.TCon n ts)
  | isClosure n                     = do ds <- currentStore
                                         let Kindle.Struct vs te _ = lookup' ds n
                                             Kindle.FunT vs' ts' t' = lookup' te (prim Code)
                                             s = vs `zip` ts
                                         return (vs', subst s ts', subst s t')
openClosureType t                   = internalError "Core2Kindle.openClosureType" t


splitClosureType 0 t0               = return ([], t0)
splitClosureType n t0               = do ([],ts,t) <- openClosureType t0
                                         (tss,t1) <- splitClosureType (n - length ts) t
                                         return (ts:tss, t1)

                                         
                                         
-- =========================================================================================
-- Translation entry point
-- =========================================================================================

-- Translate a Core.Module into a Kindle.Module
cModule e2 e3 (Module m ns xs ds ws bss)
                                    = do mapM_ addToStore (filter (isClosure . fst) e3)
                                         te0 <- tenvImp env e2
                                         ds1  <- cDecls env ds
                                         bs  <- cBindsList (addTEnv te0 (addDecls ds1 env)) bss
                                         ds2 <- currentStore
                                         let (dsQ,dsNQ) = partition (isQualified . fst) (reverse ds2)
                                             dsThis = ds1 ++ filter (isQual m . fst) dsQ
                                             ds3 = dsThis ++ dsNQ
                                         --let fromCurrent n = not (isQualified n)|| isQual m n
                                         --    ds3 = ds1++reverse (filter (fromCurrent . fst) ds2)
                                         return (Kindle.Module m ns ds3 bs,dsThis)
  where env                         = addDecls e3 (setMName m (addTEnv Kindle.primTEnv (addDecls Kindle.primDecls env0)))

-- Compute the imported type environment
tenvImp env (_,ds,ws,bs)            = cTEnv env (tsigsOf bs)


-- =========================================================================================
-- Translating Core type declarations into Kindle.Decls
-- =========================================================================================

cDecls env (Types ke ds)            = do dss <- mapM cDecl ds
                                         return (concat dss)
  where cDecl (n,DRec _ vs [] ss)   = do te <- cTEnv env ss
                                         return [(n, Kindle.Struct vs te Kindle.Top)]
        cDecl (n,DType vs t)        = return []
        cDecl (n,DData vs [] cs)    = do ds <- mapM (cCon n vs) cs
                                         return ((n, Kindle.Struct vs [] Kindle.Union) : ds)

        cCon n vs (c,Constr ts ps ke) 
                                    = do te' <- cValTEnv env te
                                         return (injectCon c, Kindle.Struct (vs++dom ke) te' (Kindle.Extends n))
          where te                  = abcSupply `zip` (ps++ts)


injectCon (Name s t m a)            = Name ('_':s) t m a
injectCon n                         = n


-- =========================================================================================
-- Translating schemes and types into Kindle-like (but still polymorphic) form
-- =========================================================================================

-- Translate a Core.Scheme into an Kindle.Type on basis of arity and polymorphism
cScheme env (Scheme rh ps ke)           = do ts0 <- mapM (cAScheme env) ps
                                             (ts,t) <- cRho env rh
                                             case (dom ke, ts0++ts) of          -- concatenate witness params with ts
                                               ([],[]) -> return (Kindle.ValT t)
                                               (vs,ts) -> return (Kindle.FunT vs ts t)
        

-- Translate a Core.Rho type
cRho env (F scs rh)                     = do ts <- mapM (cAScheme env) scs
                                             (ts',t) <- cRho' env rh
                                             return (ts++ts', t)                -- concatenate ts with monadic params
cRho env (R t)                          = cType env t


-- Translate a Core.Rho type but treat function types as closures
cRho' env (R t)                         = cType' env (tFlat t)
cRho' env rh                            = do t <- cAScheme env (Scheme rh [] [])
                                             return ([],t)


-- Translate a Core.Type
cType env (TFun ts t)                   = do ts <- mapM (cAType env) ts
                                             (ts',t) <- cType' env (tFlat t)
                                             return (ts++ts', t)                -- concatenate ts with monadic params
cType env t                             = cType' env (tFlat t)


-- Translate a Core.Type but treat function types as closures
cType' env (TId (Prim Action _), [])    = return ([t,t], Kindle.tId Msg)
  where t                               = Kindle.tId Time
cType' env (TId (Prim Request _), [t])  = do t <- cAType env t
                                             return ([Kindle.tInt], t)
cType' env (TId (Prim Class _), [t])    = do t <- cAType env t
                                             return ([Kindle.tInt], t)
cType' env (TId (Prim Cmd _), [s,t])    = do s <- cAType env s
                                             t <- cAType env t
                                             return ([Kindle.tRef s], t)
cType' env (TId (Prim PMC _), [t])      = do t <- cAType env t
                                             return ([], t)
cType' env (TId n, ts)                  = do ts <- mapM (cAType env) ts
                                             if isVar n then
                                                 return ([], Kindle.TVar n ts)
                                              else
                                                 return ([], Kindle.TCon n ts)
cType' env (TVar _, [])                 = return ([], Kindle.tInt)
cType' env (TVar _, ts)                 = do ts <- mapM (cAType env) ts
                                             return ([], Kindle.TCon (tuple (length ts)) ts)
cType' env (t, _)                       = do t <- cAType env t
                                             return ([], t)


-- Translate a Core.Type unconditionally into a Kindle.AType
cAType env t                            = do (ts, t) <- cType env t
                                             case ts of
                                                [] -> return t
                                                _  -> findClosureType env [] ts t


-- Translate a Core.Scheme unconditionally into a Kindle.AType
cAScheme env sc                         = do t0 <- cScheme env sc
                                             case t0 of
                                               Kindle.ValT t -> return t
                                               Kindle.FunT vs ts t -> findClosureType env vs ts t


-- Translate a Core.Scheme unconditionally into a ValT variant of a Kindle.Type
cValScheme env sc                       = do t <- cAScheme env sc
                                             return (Kindle.ValT t)


-- Translate a Core.TEnv into an Kindle.TEnv
cTEnv env te                            = do ts <- mapM (cScheme env) scs
                                             return (xs `zip` ts)
  where (xs,scs)                        = unzip te
       

-- Translate a Core.TEnv into an Kindle.ATEnv
cATEnv env te                           = do ts <- mapM (cAScheme env) scs
                                             return (xs `zip` ts)
  where (xs,scs)                        = unzip te
  
    
-- Translate a Core.TEnv into an Kindle.TEnv with only ValT types
cValTEnv env te                         = do ts <- mapM (cValScheme env) scs
                                             return (xs `zip` ts)
  where (xs,scs)                        = unzip te


-- =========================================================================================
-- Translating bindings
-- =========================================================================================

-- Translate a list of strongly connected Core binding groups into a list of Kindle bindings
cBindsList env []                       = return []
cBindsList env (bs:bss)                 = do (te,bf) <- cBinds env bs
                                             bs <- cBindsList (addTEnv te env) bss
                                             return (flatBinds bf ++ bs)


flatBinds bf                            = flat (bf Kindle.CBreak)
  where flat (Kindle.CBind r bs c)      = bs ++ flat c
        flat _                          = []


-- Translate a list of (mutually recursive) Core bindings into a Kindle.CBind on basis of declared type
cBinds env (Binds rec te eqs)           = do te <- cTEnv env te
                                             assert (not rec || all Kindle.okRec (rng te)) "Illegal value recursion" (dom te)
                                             (bf,bs) <- cEqs (addTEnv te env) te eqs
                                             return (te, comb rec bf bs)
  where comb False bf bs                = bf . Kindle.CBind False bs
        comb True bf bs                 = Kindle.CBind True (flatBinds bf ++ bs)


-- Translate a list of Core equations into a list of Kindle bindings on basis of declared type
cEqs env te eqs                         = do (bfs,bs) <- fmap unzip (mapM (cEq env te) eqs)
                                             return (foldr (.) id bfs, bs)

cEq env te (x,e)                        = case lookup' te x of
                                            Kindle.ValT t0 -> do
                                                (bf,e) <- cValExpT env t0 e
                                                return (bf, (x, Kindle.Val t0 e))
                                            Kindle.FunT _ ts0 _ -> do
                                                (vs,te,t,c) <- cFunN env (length ts0) e
                                                return (id, (x, Kindle.Fun vs t te c))


-- Translate a Core.Exp with a known type into a Kindle.Exp
cValExpT env t e                        = do (bf,t0,e) <- cValExp env e
                                             -- check that t == t0 here
                                             return (bf, e)
                                             
cValExpTs env [] []                     = return (id, [])
cValExpTs env (t:ts) (e:es)             = do (bf,e) <- cValExpT env t e
                                             (bf',es) <- cValExpTs env ts es
                                             return (bf . bf', e:es)


-- Translate a Core.Exp into a Kindle command, a return type, a type abstraction, and an argument list of given length
cFunN env n e                           = do (vs,te,t,c) <- cFun0 env e
                                             (te,t,c) <- adapt te t c
                                             return (vs, te, t, c)
  where adapt te t c
          | n == l_te                   = return (te, t, c)
          | n >  l_te                   = do (tss,t') <- splitClosureType (n - l_te) t
                                             te' <- newEnv paramSym (concat tss)
                                             return (te++te', t', Kindle.cmap (Kindle.multiEnter tss (map Kindle.EVar (dom te'))) c)
          | n <  l_te                   = do t1 <- findClosureType env [] (rng te2) t
                                             return (te1, t1, Kindle.CRet (Kindle.closure t1 t te2 c))
          where l_te                    = length te
                (te1,te2)               = splitAt n te


-- =========================================================================================
-- Translating abstractions
-- =========================================================================================

-- Convert a Core.Exp into a type abstraction, a parameter list and a Kindle.Cmd
cFun0 env (ELet bs e)
  | isTAbsEncoding bs                   = do (te,t,c) <- cFun env e
                                             return (tAbsVars bs, te, t, c)
cFun0 env e                             = do (te,t,c) <- cFun env e
                                             return ([], te, t, c)
                                             

-- Convert a Core.Exp into a parameter list and a Kindle.Cmd
cFun env (ELam te e)                    = do te <- cATEnv env te
                                             (te',t,c) <- cFun (addATEnv te env) e
                                             return (te ++ te', t, c)
cFun env (EReq e (EDo x tx c))          = do tx <- fmap Kindle.tRef (cAType env tx)
                                             (bf,e) <- cValExpT env tx e
                                             (t,c) <- cCmd (pushSelf x (addATEnv [(x,tx)] env)) c
                                             let bf' = Kindle.cBind [(x,Kindle.Val tx (Kindle.lock tx e))]
                                             y <- newName dummySym
                                             return ([(y,Kindle.tInt)], t, bf (bf' (Kindle.unlock x c)))
cFun env (EReq e e')                    = do (bf,tx,e) <- cValExp env e
                                             x <- newName selfSym
                                             (t,c) <- cCmdExp (pushSelf x (addATEnv [(x,tx)] env)) e'
                                             y <- newName dummySym
                                             let bf' = Kindle.cBind [(x,Kindle.Val tx (Kindle.lock tx e))]
                                             return  ([(y,Kindle.tInt)], t, bf (bf' (Kindle.unlock x c)))
cFun env e@(EAct _ _)                   = cAct env id id e
cFun env e@(EAp e0 _) 
  | isPrim After e0 || isPrim Before e0 = cAct env id id e
cFun env (ETempl x tx te c)             = do tx@(Kindle.TCon n []) <- cAType env tx  -- Type-checker guarantees tx is a struct type name
                                             te <- cValTEnv env te
                                             (t,c) <- cCmd (pushSelf x (addATEnv [(x,Kindle.tRef tx)] (addTEnv te env))) c
                                             addToStore (n, Kindle.Struct vs te Kindle.Top)
                                             y <- newName dummySym
                                             let e = Kindle.ENew (prim Ref) [tx] [(prim STATE, Kindle.Val tx (Kindle.ENew n ts []))]
                                             return ([(y,Kindle.tInt)], t, Kindle.cBind [(x,Kindle.Val (Kindle.tRef tx) e)] c)
  where vs                              = nub (tyvars te)
        ts                              = map Kindle.tVar vs
cFun env (EDo x tx c)                   = do tx <- fmap Kindle.tRef (cAType env tx)
                                             (t,c) <- cCmd (pushSelf x (addATEnv [(x,tx)] env)) c
                                             return ([(x,tx)], t, c)
cFun env e                              = do (t,r) <- cBody env e
                                             case r of
                                               FunR f ts -> do xs <- newNames paramSym (length ts)
                                                               return (xs `zip` ts, t, f (map Kindle.EVar xs))
                                               ValR c    -> return ([], t, c)


-- Translate an action expression into a Core.Cmd
cAct env fa fb (EAp e0 [e,e'])
  | isPrim After e0                     = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (te,t,c) <- cAct env (sum e1 . fa) fb e'
                                             return (te, t, bf c)
  | isPrim Before e0                    = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (te,t,c) <- cAct env fa (min e1 . fb) e'
                                             return (te, t, bf c)
  where sum (Kindle.EVar (Prim Inherit _)) a = a
        sum e1 a                             = Kindle.ECall (prim TimePlus) [] [e1,a]
        min (Kindle.EVar (Prim Inherit _)) a = a
        min e1 b                             = Kindle.ECall (prim TimeMin) [] [e1,b]
cAct env fa fb (EAct e e')              = do (_,_,c) <- cFun env (EReq e e')
                                             -- Ignore returned te (must be unused) and result type (will be replaced below)
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             m  <- newName tempSym
                                             let c1  = Kindle.cBind bs (Kindle.CRun e1 (Kindle.CRet (Kindle.EVar m)))
                                                 c2  = Kindle.cmap (\_ -> Kindle.unit) c
                                                 bs  = [(m, Kindle.Val Kindle.tMsg (Kindle.ENew (prim Msg) [] bs'))]
                                                 bs' = [(prim Code, Kindle.Fun [] Kindle.tUNIT [] c2)]
                                                 es  = [Kindle.EVar m, fa (Kindle.EVar a), fb (Kindle.EVar b)]
                                                 e1  = Kindle.ECall (prim ASYNC) [] es
                                             return ([(a,Kindle.tTime),(b,Kindle.tTime)], Kindle.tMsg, c1)
cAct env fa fb e                        = do (bf,t0,f,[ta,tb]) <- cFunExp env e
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             let c = bf (Kindle.CRet (f [fa (Kindle.EVar a), fb (Kindle.EVar b)]))
                                             return ([(a,Kindle.tTime), (b,Kindle.tTime)], Kindle.tMsg, c)


-- =========================================================================================
-- Translating let- and case expressions
-- =========================================================================================

-- Translate a Core (Pat,Exp) pair into a Kindle.Alt result
cAlt cBdy env (PLit l, e)               = do (t1,r) <- cBdy env e
                                             return (t1, rcomp (Kindle.ALit l) r)
cAlt cBdy env (PWild, e)                = do (t1,r) <- cBdy env e
                                             return (t1, rcomp Kindle.AWild r)
cAlt cBdy env (PCon k, e)               = do (vs,te,t,r) <- cRhs0 cBdy env (length te0) e
                                             return (t, rcomp (Kindle.ACon (injectCon k) vs te) r)
  where Kindle.Struct vs0 te0 _         = findDecl env k


-- Translate a Core right-hand-side into a Kindle.Cmd result, a binding, and a type abstraction
cRhs0 cBdy env n (ELet bs e)
  | isTAbsEncoding bs                   = do (te,t,r) <- cRhs cBdy env n [] e
                                             return (tAbsVars bs, te, t, r)
cRhs0 cBdy env n e                      = do (te,t,r) <- cRhs cBdy env n [] e
                                             return ([], te, t, r)
                                             

-- Translate a Core right-hand-side into a Kindle.Cmd result and a binding
cRhs cBdy env n te (ELam te' e)         = do te' <- cATEnv env te'
                                             cRhs cBdy (addATEnv te' env) n (te++te') e
cRhs cBdy env n te e
  | n == l_te                           = do (t,r) <- cBdy env e
                                             return (te, t, r)
  | n < l_te                            = do (t,r) <- cBdy env e
                                             t' <- findClosureType env [] (rng te2) t
                                             return (te1, t', rcomp (Kindle.CRet . Kindle.closure t' t te2) r)
  | n > l_te                            = do (t,r) <- cBdy env e
                                             (tss,t') <- splitClosureType (n - l_te) t
                                             te' <- newEnv paramSym (concat tss)
                                             let f = Kindle.multiEnter tss (map Kindle.EVar (dom te'))
                                             return (te++te', t', rcomp (Kindle.cmap f) r)
  where l_te                            = length te
        (te1,te2)                       = splitAt n te


-- Translate a Core.Exp into a Kindle.Cmd result
cBody env (ELet bs e)
  | not (isEncoding bs)                 = do (te,bf) <- cBinds env bs
                                             (t,r) <- cBody (addTEnv te env) e
                                             return (t, rcomp bf r)
cBody env (ECase e alts)                = cCase cBody env e alts
cBody env (EAp e0 [e]) 
  | isPrim Match e0                     = do (t,r) <- cPMC cBody env e
                                             return (t, rcomp (\c -> Kindle.CSeq c (Kindle.CRaise (Kindle.ELit (lInt 1)))) r)
cBody env e                             = do (bf,t,h) <- cExp env e
                                             case h of
                                               ValR e    -> return (t, ValR (bf (Kindle.CRet e)))
                                               FunR f ts -> return (t, FunR (bf . Kindle.CRet . f) ts)


-- Note: we don't really handle PMC terms as first class citizens, rather like constructors in a small grammar
-- of pattern-matching expressions:
-- e  ::=  ...  |  Match pm
-- pm ::=  Commit e  |  Fail  |  Fatbar pm pm  |  case e of {p -> pm} pm  |  let bs in pm
-- This syntax is followed when PMC terms are introduced in module Match, and is also respected by Termred.
--
-- However, should we for some reason want to allow abstraction over PMC terms, as in (\e -> Commit e),
-- the translation below will need to be complemented with a concrete implementation of the PMC type constructor
-- (using Maybe, for example), and corresponding general implementations of Match, Commit, Fail & Fatbar.


-- Translate a Core.Exp corresponding to a PMC term into a Kindle.Cmd result
cPMC cE env (ELet bs e)
  | not (isEncoding bs)                 = do (te,bf) <- cBinds env bs
                                             (t,r) <- cPMC cE (addTEnv te env) e
                                             return (t, rcomp bf r)
cPMC cE env (ECase e alts)              = cCase (cPMC cE) env e alts
cPMC cE env (EAp e0 [e1,e2])
  | isPrim Fatbar e0                    = do r1 <- cPMC cE env e1
                                             r2 <- cPMC cE env e2
                                             let n = maximum [rArity r1, rArity r2]
                                             r1 <- adaptR env n r1
                                             r2 <- adaptR env n r2
                                             return (mkSeq r1 r2)
cPMC cE env (EAp e0 [e])
  | isPrim Commit e0                    = cE env e
cPMC cE env e0
  | isPrim Fail e0                      = do [t] <- cTArgs env e0
                                             return (t, ValR Kindle.CBreak)
cPMC cE env e                           = internalError "PMC syntax violated in Core2Kindle" e


-- Translate the parts of a case expression into a Kindle.Cmd result
cCase cE env e ((PCon k,e'):_)
  | isTuple k                           = do (bf,_,e) <- cValExp env e
                                             (te,t1,r) <- cRhs cE env (width k) [] e'
                                             let (xs,ts) = unzip te
                                                 bs = mkBinds xs ts (map (Kindle.ESel e) (take (width k) abcSupply))
                                             return (t1, rcomp (bf . Kindle.cBind bs) r)
cCase cE env e alts                     = do (bf,t0,e0) <- cValExp env e
                                             rs <- mapM (cAlt cE env) alts
                                             let n = maximum (map rArity rs)
                                             rs <- mapM (adaptR env n) rs
                                             return (mkSwitch bf rs e0)


adaptR env 0 (t0, ValR c)               = return (t0, ValR c)
adaptR env n (t0, FunR f ts)
  | n == l_ts                           = return (t0, FunR f ts)
  | n > l_ts                            = do (tss,t) <- splitClosureType (n - l_ts) t0
                                             return (t, FunR (g tss) (ts ++ concat tss))
  where l_ts                            = length ts
        g tss es                        = let (es1,es2) = splitAt l_ts es
                                          in Kindle.cmap (Kindle.multiEnter tss es2) (f es1)
adaptR env n (t0, ValR c)               = do (tss,t) <- splitClosureType n t0
                                             return (t, FunR (g tss) (concat tss))
  where g tss es                        = Kindle.cmap (Kindle.multiEnter tss es) c


rArity (t, ValR _)                      = 0
rArity (t, FunR f ts)                   = length ts

mkSwitch bf ((t,ValR c):rs) e0          = (t, ValR (bf (Kindle.CSwitch e0 alts)))
  where alts                            = c : [ alt | (_,ValR alt) <- rs ]     
mkSwitch bf ((t,FunR g ts):rs) e0       = (t, FunR (\es -> bf (Kindle.CSwitch e0 (map ($es) (g:gs)))) ts)
  where gs                              = [ g | (_,FunR g _) <- rs ]

mkSeq (t1,ValR c1) (t2,ValR c2)         = (t1, ValR (Kindle.CSeq c1 c2))
mkSeq (t1,FunR g1 ts1) (t2,FunR g2 ts2) = (t1, FunR (\es -> Kindle.CSeq (g1 es) (g2 es)) ts1)


-- =========================================================================================
-- Translating commands
-- =========================================================================================

-- Translate a Core.Cmd into a Kindle.Cmd
cCmd env (CRet e)                       = do (bf,te,e) <- freezeState env e
                                             (bf',t,e) <- cValExp (addTEnv te env) e
                                             if Kindle.simpleExp e then         -- No state references or non-termination in e
                                                 return (t, bf (bf' (Kindle.CRet e)))   -- Can be ignored (see CGen alternative)
                                              else do
                                                 x <- newName tempSym
                                                 let bf'' = Kindle.cBind [(x, Kindle.Val t e)]
                                                 return (t, bf (bf' (bf'' (Kindle.CRet (Kindle.EVar x)))))
cCmd env (CAss x e c)                   = do (bf,te,e) <- freezeState env e
                                             (bf',e) <- cValExpT (addTEnv te env) tx e
                                             (t,c) <- cCmd env c
                                             return (t, bf (bf' (Kindle.CUpdS (stateRef env) x e c)))
  where Kindle.ValT tx                  = lookup' (tenv env) x
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
                                             tx <- cAType env tx
                                             (bf',e) <- cValExpT (addTEnv te env) tx (EAp e [EVar (self env)])
                                             (t,c) <- cCmd (addATEnv [(x,tx)] env) c
                                             return (t, bf (bf' (Kindle.cBind [(x,Kindle.Val tx e)] c)))
cCmd env (CExp e)                       = cCmdExp env e


-- Translate a Core.Exp in the monadic execution path into a Kindle.Cmd
cCmdExp env (ELet bs e)
  | not (isEncoding bs)                 = do (bf,te,bs) <- freezeState env bs
                                             (te',bf') <- cBinds (addTEnv te env) bs
                                             (t,c) <- cCmdExp (addTEnv te' env) e
                                             return (t, bf (bf' c))
cCmdExp env (EAp e0 [e])  
--  | isPrim ReqToCmd e0                  = ...
  | isPrim Raise e0                     = do (bf,te,e) <- freezeState env e
                                             (bf',t,e') <- cValExp (addTEnv te env) e
                                             return (t, bf (bf' (Kindle.CRaise e')))
  | isPrim Match e0                     = do (bf,te,e) <- freezeState env e
                                             (t,ValR c) <- cPMC cValCmdExp (addTEnv te env) e
                                             return (t, bf (Kindle.CSeq c (Kindle.CRaise (Kindle.ELit (lInt 1)))))
cCmdExp env (EDo x tx c)                = do tx <- cAType env tx
                                             (t,c) <- cCmd (pushSelf x (addATEnv [(x,Kindle.tRef tx)] env)) c
                                             return (t, Kindle.cBind [(x,Kindle.Val (Kindle.tRef tx) (Kindle.EVar (self env)))] c)
cCmdExp env (ECase e alts)              = do (bf,te,e) <- freezeState env e
                                             (t,ValR c) <- cCase cValCmdExp (addTEnv te env) e alts
                                             return (t, bf c)
cCmdExp env e                           = do (bf,te,e) <- freezeState env e
                                             (bf',t,e) <- cValExp (addTEnv te env) (EAp e [EVar (self env)])
                                             x <- newName tempSym
                                             let bf'' = Kindle.cBind [(x, Kindle.Val t e)]
                                             return (t, bf (bf' (bf'' (Kindle.CRet (Kindle.EVar x)))))


cValCmdExp env e                        = do (t,c) <- cCmdExp env e
                                             return (t, ValR c)
                                             

-- State variables are normally translated into field selections from the current "self".  For example,
--     x := 7; result (x + 1)
-- gets translated into
--     self->x := 7; result (self->x + 1)
-- However, closure values, which may be invoked long after they are defined, must not be sensitive to state
-- mutations.  This means that "self" dereferencing operations for any contained state variables must be done 
-- when the closure is defined, not when it is invoked.  Here's a challenging example:
--     x := 7; f = \y->x+y; x := 2; result (f 1);
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


freezeState env xx
  | null vs                             = return (id, [], xx)
  | otherwise                           = do vs' <- newNames paramSym (length vs)
                                             return (Kindle.cBind (zipWith3 f vs' ts vs), vs' `zip` ts, subst (vs `zip` map EVar vs') xx)
  where vs                              = nub (fragile xx)
        ts                              = map (lookup' (tenv env)) vs
        f v' (Kindle.ValT t) v          = (v', Kindle.Val t (Kindle.ESel (stateRef env) v))


-- Return a Kindle expression identifying the current state struct
stateRef env                            = Kindle.ESel (Kindle.EVar (self env)) (prim STATE)


-- =========================================================================================
-- Translating expressions
-- =========================================================================================

-- Translate a Core.Exp into an expression result that is either a value or a function,
-- overflowing into a list of Kindle.Binds if necessary
cExp env (ELet bs e)
  | isTAppEncoding bs                   = do ts <- mapM (cAType env) (tAppTypes bs)
                                             cExp (setTArgs env ts) e
  | not (isTAbsEncoding bs)             = do (te,bf) <- cBinds env bs
                                             (bf',t,h) <- cExp (addTEnv te env) e
                                             return (bf . bf', t, h)
cExp env (ELit l)                       = return (id, Kindle.litType l, ValR (Kindle.ELit l))
cExp env (ERec c eqs)                   = do (bf,bs) <- cEqs (setTArgs env []) te' eqs
                                             return (bf, Kindle.TCon c ts, ValR (Kindle.ENew c ts bs))
  where ts                              = tArgs env
        Kindle.Struct vs te _           = findDecl env c
        te'                             = subst (vs `zip` ts) te
cExp env (EAp e0 [e])
  | isPrim ActToCmd e0                  = do (bf,t,e) <- cValExp env (EAp e [EVar (prim Inherit), EVar (prim Inherit)])
                                             [t1] <- cTArgs env e0
                                             return (bf, t, FunR (\_ -> e) [t1])
  | isPrim ReqToCmd e0                  = do (bf,t,e) <- cValExp env (EAp e [ELit (lInt 0)])
                                             [t1,t2] <- cTArgs env e0
                                             return (bf, t, FunR (\_ -> e) [t2])
  | Just t <- isCastPrim e0             = do (bf,t,e) <- cExp env e
                                             return (bf, t, rcomp (Kindle.ECast t) e)
  | isPrim RefToPID e0                  = do (bf,t,e) <- cExp env e
                                             return (bf, Kindle.tPID, rcomp (Kindle.ECast Kindle.tPID) e)
  | isPrim New e0                       = cExp env (EAp e [ELit (lInt 0)])       -- Can't occur but in CBind rhs, syntactic restriction
cExp env (EAp e0 [e,e'])
  | isPrim After e0                     = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (bf',t,f,ts) <- cFunExp env e'
                                             return (bf . bf', t, FunR (\[a,b] -> f [sum a e1, b]) ts)
  | isPrim Before e0                    = do (bf,e1) <- cValExpT env Kindle.tTime e
                                             (bf',t,f,ts) <- cFunExp env e'
                                             return (bf . bf', t, FunR (\[a,b] -> f [a, min b e1]) ts)
  where sum a e1                        = Kindle.ECall (prim TimePlus) [] [a,e1]
        min b e1                        = Kindle.ECall (prim TimeMin) [] [b,e1]
cExp env (EAp e es) 
  | not (isPrim Match e)                = do (bf,t,f,ts) <- cFunExp env e
                                             appFun env bf t f ts es
  where appFun env bf t f ts es
          | l_ts <  l_es                = do (bf',es1) <- cValExpTs env ts es1
                                             (vs,ts',t') <- openClosureType t 
                                             appFun env (bf . bf') t' (Kindle.enter (f es1) []) ts' es2
          | l_ts == l_es                = do (bf',es) <- cValExpTs env ts es
                                             return (bf . bf', t, ValR (f es))
          | l_ts >  l_es                = do (bf',es) <- cValExpTs env ts1 es
                                             return (bf . bf', t, FunR (f . (es++)) ts2)
          where l_ts                    = length ts
                l_es                    = length es
                (ts1,ts2)               = splitAt l_es ts
                (es1,es2)               = splitAt l_ts es
cExp env (EVar x)                       = case lookup' (tenv env) x of
                                             Kindle.ValT t        -> return (id, t, ValR e)
                                             Kindle.FunT vs ts' t 
                                               | null ts'         -> return (id, subst s t, ValR (Kindle.ECall x ts []))
                                               | otherwise        -> return (id, subst s t, FunR (Kindle.ECall x ts) (subst s ts'))
                                               where s             = vs `zip` ts
  where e                               = if stateVar (annot x) then Kindle.ESel (stateRef env) x else Kindle.EVar x
        ts                              = tArgs env
cExp env (ESel e l)                     = do (bf,e) <- cValExpT (setTArgs env []) (Kindle.TCon k ts0) e
                                             case subst (vs0 `zip` ts0) rhstype of
                                               Kindle.ValT t       -> return (bf, t, ValR (Kindle.ESel e l))
                                               Kindle.FunT vs ts t -> return (bf, subst s t, FunR (Kindle.EEnter e l ts1) (subst s ts))
                                                 where s            = vs `zip` ts1
  where (k,vs0,rhstype)                 = Kindle.typeOfSel (decls env) l
        (ts0,ts1)                       = splitAt (length vs0) (tArgs env)  -- tArgs lists *full* instantiation, not just local quantification
cExp env (ECon k)                       = case te of
                                             [] -> return (id, t0, ValR (newK []))
                                             _  -> return (id, t0, FunR (newK . mkBinds abcSupply ts') ts')
  where ts                              = tArgs env
        Kindle.Struct vs te _           = findDecl env k
        ts'                             = subst (vs `zip` ts) (map (Kindle.rngType . snd) te)
        (k0,n)                          = Kindle.typeOfCon (decls env) k
        t0                              = Kindle.TCon k0 (take n ts)
        newK | isTuple k                = Kindle.ENew k ts
             | otherwise                = Kindle.ECast t0 . Kindle.ENew (injectCon k) ts
cExp env e                              = do (vs,te,t,c) <- cFun0 env e
                                             case (vs,te) of
                                               ([],[]) -> do
                                                  x <- newName tempSym
                                                  return (Kindle.cBind [(x, Kindle.Fun [] t [] c)], t, ValR (Kindle.ECall x [] []))
                                               _  -> do 
                                                  t' <- findClosureType env vs (rng te) t
                                                  return (id, t', ValR (Kindle.closure2 t' vs t te c))


-- Translate a Core.Exp into a Kindle value expression
cValExp env e                           = do (bf,t,h) <- cExp env e
                                             case h of
                                               ValR e -> 
                                                  return (bf, t, e)
                                               FunR f ts -> do
                                                  xs <- newNames paramSym (length ts)
                                                  t' <- findClosureType env [] ts t
                                                  let es = map Kindle.EVar xs
                                                      te = xs `zip` ts
                                                  return (bf, t', Kindle.closure t' t te (Kindle.CRet (f es)))


-- Translate a Core.Exp into a Kindle function
cFunExp env e                           = do (bf,t,h) <- cExp env e
                                             case h of
                                               FunR f ts -> return (bf, t, f, ts)
                                               ValR e' -> do ([],ts,t') <- openClosureType t
                                                             return (bf, t', Kindle.enter e' [], ts)
 

-- Map a Kindle.ATEnv (unzipped) and a list of Kindle.Exps into a list of Kindle.Binds
mkBinds xs ts es                        = zipWith3 f xs ts es
  where f x t e                         = (x, Kindle.Val t e)


-- Check if expression is a primitive, possibly applied to type arguments
isPrim p (ELet bs e) 
  | isTAppEncoding bs                   = isPrim p e
isPrim p (EVar (Prim p' _))
  | p == p'                             = True
isPrim p e                              = False

isCastPrim (EVar (Prim p _))
  | p `elem` intCasts                   = Just Kindle.tInt
  | p == IntToChar                      = Just Kindle.tChar
  | p == IntToBITS8                     = Just Kindle.tBITS8
  | p == IntToBITS16                    = Just Kindle.tBITS16
  | p == IntToBITS32                    = Just Kindle.tBITS32
  where intCasts                        = [CharToInt,BITS8ToInt,BITS16ToInt,BITS32ToInt]
isCastPrim _                            = Nothing


-- Extract type arguments from expression
cTArgs env (ELet bs e)
  | isTAppEncoding bs                   = mapM (cAType env) (tAppTypes bs)
cTArgs env e                            = return []



-- Additional entry point for translating imported environments
c2kTEnv ds te                         = localStore f
  where f                               = do mapM_ addToStore (filter (isClosure . fst) ds)
                                             te <- cTEnv env0 te
                                             return (filter p te)
--        p (_,Kindle.FunT _ _ (Kindle.TCon n _))    
--                                        = not (isClosure n) -- ???
        p _                             = True
