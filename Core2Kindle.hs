module Core2Kindle(core2kindle) where
{- -}
import Common
import Core
import Name
import Depend
import qualified Kindle


core2kindle                         :: Module -> M s Kindle.Module
core2kindle m                       = localStore (cModule m)

-- Note: bound variables in the output from this pass are no longer guaranteed to be globally unique!
-- This arises from the handling of orphaned state variables (see bindOrphans)


data Env                            = Env { decls :: Kindle.Decls,
                                            tenv  :: Kindle.TEnv,
                                            cons  :: Map Name Name,     -- connects constructor names to their sum type
                                            selfN :: Maybe Name }

env0                                = Env { decls = Kindle.primDecls, 
                                            tenv  = Kindle.primTEnv,
                                            cons  = Kindle.primCons,
                                            selfN = Nothing }

addDecls ds env                     = env { decls = ds ++ decls env }

addTEnv te env                      = env { tenv = te ++ tenv env }

addTEnv' te env                     = addTEnv (mapSnd Kindle.ValT te) env

addCons cs env                      = env { cons = cs ++ cons env }

pushSelf x t env                    = env { tenv = (x,Kindle.ValT t) : tenv env, selfN = Just x }

haveSelf env                        = selfN env /= Nothing

self env                            = fromJust (selfN env)

nullSelf env                        = env { selfN = Nothing }

findSel env l                       = head (Kindle.searchFields (decls env) l)

findCon env k                       = case lookup k (decls env) of
                                        Just (Kindle.Struct te) -> (t0, Just (mapSnd valT te))
                                        _                       -> (t0, Nothing)
  where t0                          = Kindle.TId (substVar (cons env) k)
        valT (Kindle.ValT t)        = t
        valT (Kindle.FunT _ _)      = error "Internal: c2k.findCon"     -- can't happen, see cCon below


-- Convert a Core.Modul into a Kindle.Module
cModule (Module m ds _ bs)          = do ds1  <- cDecls ds
                                         mapM_ addToStore (filter (isClosure . fst) ds1)
                                         bs  <- cBindsList (addDecls ds1 env) (groupBinds bs)
                                         ds2 <- currentStore
                                         return (Kindle.Module m (ds1++reverse ds2) bs)
  where env                         = addCons (dataCons ds) env0


-- Build a mapping from constructor names to the corresponding datatype name
dataCons (Types ke ds)              = concat [ dom cs `zip` repeat c | (c,DData _ _ cs) <- ds ]


-- Convert a list of strongly connected Core binding groups into a list of Kindle bindings
cBindsList env []                     = return []
cBindsList env (bs:bss)               = do (te,bf) <- cBinds env bs
                                           bs <- cBindsList (addTEnv te env) bss
                                           return (flat (bf Kindle.CBreak) ++ bs)
  where flat (Kindle.CBind r bs c)    = bs ++ flat c        -- temporary...
        flat _                        = []


findClosureName ts t                = do ds <- currentStore
                                         case find ds of
                                            Just n -> return n
                                            Nothing -> do
                                               n <- newName closureSym
                                               addToStore (n, Kindle.Struct [(prim Code, Kindle.FunT ts t)])
                                               return n
  where find []                     = Nothing
        find ((n,Kindle.Struct [(x,Kindle.FunT ts' t')]) : ds)  |  isClosure n && ts == ts' && t == t'     
                                    = Just n
        find (_ : ds)               = find ds


findClosureDef n                    = do ds <- currentStore
                                         case lookup n ds of
                                            Just (Kindle.Struct [(Prim Code _, Kindle.FunT ts t)]) -> return (ts,t)
                                            _ -> error "Internal: c2k.findClosureDef"


simple (c,Constr ts ps _)           = null ts && null ps


-- Convert a Core.Types into a Kindle.Decls
cDecls (Types ke ds)                = do dss <- mapM cDecl ds
                                         return (concat dss)
  where cDecl (n,DRec _ vs [] ss)   = do te <- cTEnv ss
                                         return [(n, Kindle.Struct te)]
        cDecl (n,DType vs t)        = error ("Internal: cannot yet compile type synonyms ")
        cDecl (n,DData vs [] cs)
          | all simple cs           = return [(n, Kindle.Enum (dom cs))]
          | otherwise               = do tag <- newName tagSym
                                         n' <- newName typeSym
                                         let te0 = [(tag, Kindle.ValT (Kindle.TId n'))]
                                         ds <- mapM (cCon te0) cs
                                         return ((n', Kindle.Enum (dom cs)) : (n, Kindle.Struct te0) : ds)

        cCon te0 (c,Constr ts ps _) = do te <- newEnv paramSym (ps++ts)
                                         te <- cTEnv' te
                                         return (c, Kindle.Struct (te0++te))

-- Convert a Core.TEnv into a Kindle.TEnv on basis of type arity
cTEnv te                            = mapM conv te
  where conv (x,t)                  = do (ts, t') <- cType (deQualify t)
                                         case ts of
                                           [] -> return (x, Kindle.ValT t')
                                           _  -> return (x, Kindle.FunT ts t')

-- Convert a Core.TEnv into a Kindle.TEnv with only atomic types
cTEnv' te                           = do te' <- cATEnv te
                                         return (mapSnd Kindle.ValT te')



-- Convert a Core.TEnv into a Kindle.ATEnv
cATEnv te                             = mapM conv te
  where conv (x,s)                    = do t <- cAType (deQualify s)
                                           return (x,t)


-- Reduce schemes and rho types to ordinary (function) types by simply removing the quantifiers, leaving the tyvars free.
deQualify (Scheme rh [] ke)         = deQualify' rh
deQualify (Scheme rh ps ke)         = case deQualify' rh of
                                        TFun ts t -> TFun (map deQualify ps ++ ts) t    -- concatenate ps with topmost params
                                        t         -> TFun (map deQualify ps) t
                                        

deQualify' (R t)                    = t
deQualify' (F ts rh)                = TFun (map deQualify ts) (deQualify' rh)


-- Convert a Core.Type into a (possibly empty) list of Kindle.AType parameters and a return Kindle.AType
cType (TFun ts t)                   = do ts <- mapM cAType ts
                                         (ts',t) <- cType' t
                                         return (ts++ts', t)                        -- concatenate ts with any monadic params
cType t                             = cType' t


cType' (TId (Prim Action _))              = return ([t,t], Kindle.TId (prim Msg))
  where t                                 = Kindle.TId (prim Time)
cType' (TAp (TId (Prim Request _)) t)     = do t <- cAType t
                                               return ([Kindle.TWild], t)
cType' (TAp (TId (Prim Template _)) t)    = do t <- cAType t
                                               return ([Kindle.TWild], t)
cType' (TAp (TAp (TId (Prim Cmd _)) s) t) = do s <- cAType s
                                               t <- cAType t
                                               return ([s], t)
cType' (TAp (TId (Prim Ref _)) t)         = do t <- cAType t
                                               return ([], t)
cType' (TAp t t')                         = cType' t
cType' (TId n)
  | isCon n                               = return ([], Kindle.TId n)
  | otherwise                             = return ([], Kindle.TWild)
cType' (TVar _)                           = return ([], Kindle.TWild)


-- Convert a Core.Type into a Kindle.AType, overflowing into a list of Kindle.Decls if necessary
cAType t                              = do (ts, t) <- cType t
                                           case ts of
                                             [] -> return t
                                             ts -> do n <- findClosureName ts t
                                                      return (Kindle.TId n)


-- Convert a (mutually recursive) set of Core bindings into a list of Kindle bindings on basis of declared type
cBinds env (Binds r te eqs)           = do te <- cTEnv te
                                           assert (not r || all rec (rng te)) "Illegal value recursion"
                                           (bf,bs) <- cEqs (if r then addTEnv te env else env) te eqs
                                           return (te, bf . Kindle.CBind r bs)
  where rec (Kindle.FunT ts t)        = True                            -- Good: function
        rec (Kindle.ValT t)           = rec1 t
        rec1 (Kindle.TWild)           = False                           -- Bad: black hole
        rec1 (Kindle.TId n)           = rec2 (lookup n (decls env))
        rec2 (Just (Kindle.Struct _)) = True                            -- Good: heap-allocated object
        rec2 _                        = False                           -- Bad: primitive type or enum type


-- Convert a set of Core equations into a list of Kindle bindings on basis of declared type
cEqs env te eqs                       = do (bfs,bs) <- fmap unzip (mapM cEq eqs)
                                           return (foldr (.) id bfs, bs)
  where cEq (x,e)                     = case lookup' te x of
                                          Kindle.ValT t -> do
                                              (bf,e) <- cExpT env t e
                                              return (bf, (x, Kindle.Val t e))
                                          Kindle.FunT ts t -> do
                                              (te,c) <- cFunT (nullSelf env) ts t e             -- Top-down mode!
                                              return (id, (x, Kindle.Fun t te c))


-- =========================================================================================
-- Top-down mode
-- =========================================================================================

-- Convert a Core.Exp with an expected Kindle function type into a parameter list and a Kindle.Cmd
cFunT env ts t0 (ELam te e)
  | l_ts < l_te                         = error "Internal: c2k.cFunT"
  | l_ts >= l_te                        = do (te',c) <- cFunT (addTEnv' te1 env) ts2 t0 e
                                             return (te1++te', c)
  where te1                             = dom te `zip` ts1
        (ts1,ts2)                       = splitAt l_te ts
        l_ts                            = length ts
        l_te                            = length te
cFunT env [ty] t0 (EReq e e')           = do (bf,tx,e) <- cExp env e
                                             x <- newName selfSym
                                             c <- cCmdExpT (pushSelf x tx env) t0 e'
                                             c <- Kindle.protect x t0 c
                                             y <- newName dummySym
                                             return ([(y,ty)], bf (Kindle.cBind [(x,Kindle.Val tx e)] c))
cFunT env [_,_] t0 e@(EAct _ _)         = cAct env id id e
cFunT env [_,_] t0 e@(EAp (EVar (Prim After _) _) _) 
                                        = cAct env id id e
cFunT env [_,_] t0 e@(EAp (EVar (Prim Before _) _) _) 
                                        = cAct env id id e
cFunT env [ty] t0 (ETempl x tx te c)    = do tx@(Kindle.TId n) <- cAType tx     -- Type-checker guarantees tx is a struct type name
                                             te <- cTEnv' te
                                             addToStore (n, Kindle.Struct te)
                                             c <- cCmdT (pushSelf x tx (addTEnv te env)) t0 c
                                             y <- newName dummySym
                                             return ([(y,ty)], Kindle.cBind [(x,Kindle.Val tx (Kindle.ENew n []))] c)
cFunT env [tx] t0 (EDo x tx' c)         = do c <- cCmdT (pushSelf x tx env) t0 c
                                             return ([(x,tx)], c)
cFunT env ts t0 e                       = do te <- newEnv paramSym ts
                                             c <- cBodyT (addTEnv' te env) t0 (eAp e (map eVar (dom te)))
                                             return (te, c)


-- Convert a Core (Pat,Exp) pair with an expected Kindle.AType and a scrutinee expression into Kindle.Alt
cAltT cBodyT env t0 e0 (PLit l, e)      = do c <- cBodyT env t0 e
                                             return (Kindle.ALit l c)
cAltT cBodyT env t0 e0 (PCon n, e)      = case findCon env n of
                                            (_,Just te) -> do let e1 = Kindle.ECast (Kindle.TId n) e0
                                                                  (xs,ts) = unzip (tail te)
                                                              c <- cRhsT env t0 e ts (map (Kindle.ESel e1) xs)
                                                              return (Kindle.ACon n c)
                                            (_,Nothing) -> do c <- cBodyT env t0 e
                                                              return (Kindle.ACon n c)
  where cRhsT env t0 e [] []            = cBodyT env t0 e
        cRhsT env t0 (ELam te e) ts es  = do te <- cATEnv te
                                             let bs = mkBind te (zipWith3 match (rng te) ts1 es1)
                                             c <- cRhsT (addTEnv' te env) t0 e ts2 es2
                                             return (Kindle.cBind bs c)
          where (es1,es2)               = splitAt (length te) es
                (ts1,ts2)               = splitAt (length te) ts
        cRhsT env t0 e ts es            = do te <- newEnv paramSym ts
                                             let bs = mkBind te es
                                             c <- cBodyT (addTEnv' te env) t0 (EAp e (map eVar (dom te)))
                                             return (Kindle.cBind bs c)

      
-- Convert a Core.Exp with an expected Kindle.AType into a Kindle.Cmd
cBodyT env t0 (ELet bs e)               = do (te,bf) <- cBinds env bs
                                             c <- cBodyT (addTEnv te env) t0 e
                                             return (bf c)
cBodyT env t0 (ECase e alts e')         = do (bf,t,e0) <- cExp env e
                                             alts <- mapM (cAltT cBodyT env t0 e0) alts
                                             c <- cBodyT env t0 e'
                                             return (bf (Kindle.CSwitch (scrutinee env t e0) alts c))
cBodyT env t0 (EAp (EVar (Prim Refl _) _) [e])      = cBodyT env t0 e
cBodyT env t0 (EAp (EVar (Prim Match _) _) [e])     = cBodyT env t0 e
cBodyT env t0 (EAp (EVar (Prim Commit _) _) [e])    = cBodyT env t0 e
cBodyT env t0 (EVar (Prim Fail _) _)                = return Kindle.CBreak
cBodyT env t0 (EAp (EVar (Prim Fatbar _) _) [e,e']) = do c <- cBodyT env t0 e
                                                         c' <- cBodyT env t0 e'
                                                         return (Kindle.CSeq c c')
cBodyT env t0 e                         = do (bf,e) <- cExpT env t0 e
                                             return (bf (Kindle.CRet e))



-- Convert a Core.Exp with a known Kindle.AType into a Core.Exp
cExpT env t0 e0                         = do (bf,t,e) <- cExp env e0
                                             r <- matches t0 t
                                             if r then
                                                 return (bf, match t0 t e)
                                              else
                                                 cExpT' t0 e0
  where matches (Kindle.TId n) (Kindle.TId n')
          | isClosure n && isClosure n' = do (ts,t)   <- findClosureDef n
                                             (ts',t') <- findClosureDef n'
                                             if length ts /= length ts' then 
                                                 return False
                                              else
                                                 fmap and (sequence (zipWith matches (t:ts) (t':ts')))
        matches t t'                    = return True
        cExpT' (Kindle.TId n) e         = do (ts,t) <- findClosureDef n
                                             (te,c) <- cFunT (nullSelf env) ts t e
                                             return (id, Kindle.ENew n [(prim Code, Kindle.Fun t te c)])


cExpTs env [] []                        = return (id, [])
cExpTs env (t:ts) (e:es)                = do (bf,e) <- cExpT env t e
                                             (bf',es) <- cExpTs env ts es
                                             return (bf . bf', e:es)


-- Convert a Core.Cmd into a Kindle.Cmd
cCmdT env t0 (CRet e)                   = do (bf,e) <- cExpT env t0 e
                                             return (bindOrphans env bf e (Kindle.CRet e))
cCmdT env t0 (CAss x e c)               = do (bf,e) <- cExpT env tx e
                                             (c) <- cCmdT env t0 c
                                             return (bindOrphans env bf e (Kindle.CAssign (Kindle.EVar (self env)) x e c))
  where tx                              = Kindle.rngType (lookup' (tenv env) x)
cCmdT env t0 (CLet bs c)                = do (te,bf) <- cBinds env bs
                                             c <- cCmdT (addTEnv te env) t0 c
                                             return (bindOrphans' env bf c)
cCmdT env t0 (CGen x tx e c)
  | isDummy x                           = do (bf,t,e) <- cExp env (EAp e [eVar (self env)])
                                             c <- cCmdT env t0 c
                                             return (bindOrphans env bf e (Kindle.CRun e c))
  | otherwise                           = do tx <- cAType tx
                                             (bf,e) <- cExpT env tx (EAp e [eVar (self env)])
                                             c <- cCmdT (addTEnv [(x,Kindle.ValT tx)] env) t0 c
                                             return (bindOrphans env bf e (Kindle.cBind [(x,Kindle.Val tx e)] c))
cCmdT env t0 (CExp e)                   = cCmdExpT env t0 e


-- Convert a Core.Exp in the execution path into a Kindle.Cmd
cCmdExpT env t0 (EDo x tx c)            = do tx <- cAType tx
                                             c <- cCmdT (pushSelf x tx env) t0 c
                                             return (Kindle.cBind [(x,Kindle.Val tx (Kindle.EVar (self env)))] c)
cCmdExpT env t0 (ECase e alts e')       = do (bf,t,e0) <- cExp env e
                                             alts <- mapM (cAltT cCmdExpT env t0 e0) alts
                                             c <- cBodyT env t0 e'
                                             return (bindOrphans env bf e0 (Kindle.CSwitch (scrutinee env t e0) alts c))
cCmdExpT env t0 e                       = do (bf,e) <- cExpT env t0 (EAp e [eVar (self env)])
                                             return (bindOrphans env bf e (Kindle.CRet e))


-- =========================================================================================
-- Misc helpers
-- =========================================================================================

bindOrphans env bf e
  | not (haveSelf env)                  = bf
  | otherwise                           = Kindle.cBind bs' . bf
  where bs'                             = zipWith mkSelBind vs ts
        vs                              = nub (filter isState (idents e ++ idents (bf Kindle.CBreak)))
        ts                              = map (Kindle.rngType . snd . findSel env) vs
        mkSelBind v t                   = (v, Kindle.Val t (Kindle.ESel (Kindle.EVar (self env)) v))
 
bindOrphans' env bf                     = bindOrphans env bf (Kindle.EVar (prim UNIT))
 

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
                                        = do (bf,e1) <- cExpT env tTime e
                                             (te,c) <- cAct env (sum e1 . fa) fb e'
                                             return (te, bf c)
  where sum e1 a                        = Kindle.ECall (prim TimePlus) [e1,a]
cAct env fa fb (EAp (EVar (Prim Before _) _) [e,e'])
                                        = do (bf,e1) <- cExpT env tTime e
                                             (te,c) <- cAct env fa (min e1 . fb) e'
                                             return (te, bf c)
  where min e1 b                        = Kindle.ECall (prim TimeMin) [e1,b]
cAct env fa fb (EAct e e')              = do (ignore_te,c) <- cFunT env [Kindle.TWild] Kindle.TWild (EReq e e')
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             m  <- newName tempSym
                                             let c'  = Kindle.cBind bs (Kindle.CRun e1 (Kindle.CRet (Kindle.EVar m)))
                                                 bs  = [(m, Kindle.Val tMsg (Kindle.ENew (prim Msg) bs'))]
                                                 bs' = [(prim Code, Kindle.Fun Kindle.TWild [] c)]
                                                 es  = [Kindle.EVar m, fa (Kindle.EVar a), fb (Kindle.EVar b)]
                                                 e1  = Kindle.ECall (prim ASYNC) es
                                             return ([(a,tTime),(b,tTime)], c')
cAct env fa fb e                        = do (bf,t0,f,[ta,tb]) <- cFHead env e
                                             a  <- newName paramSym
                                             b  <- newName paramSym
                                             let c = bf (Kindle.CRet (f [fa (Kindle.EVar a), fb (Kindle.EVar b)]))
                                             return ([(a,tTime), (b,tTime)], c)

tTime                                   = Kindle.TId (prim Time)
tMsg                                    = Kindle.TId (prim Msg)

cAct' env fa fb e                       = do (te,c) <- cAct env fa fb e
                                             return (te, tMsg, c)
                                             

-- =========================================================================================
-- Bottom-up mode
-- =========================================================================================

-- Convert a Core.Exp into a parameter list, a Kindle.AType and a Kindle.Cmd
cFun env (ELam te e)                    = do te <- cATEnv te
                                             (te',t,c) <- cFun (addTEnv' te env) e
                                             return (te++te', t, c)
cFun env (EReq e e')                    = do (bf,tx,e) <- cExp env e
                                             x <- newName selfSym
                                             (t,c) <- cCmdExp (pushSelf x tx env) e'
                                             c <- Kindle.protect x t c
                                             y <- newName dummySym
                                             return ([(y,Kindle.TWild)], t, bf (Kindle.cBind [(x,Kindle.Val tx e)] c))
cFun env e@(EAct _ _)                   = cAct' env id id e
cFun env e@(EAp (EVar (Prim After _) _) _) 
                                        = cAct' env id id e
cFun env e@(EAp (EVar (Prim Before _) _) _) 
                                        = cAct' env id id e
cFun env (ETempl x tx te c)             = do tx@(Kindle.TId n) <- cAType tx     -- Type-checker guarantees tx is a struct type name
                                             te <- cTEnv' te
                                             addToStore (n, Kindle.Struct te)
                                             (t,c) <- cCmd (pushSelf x tx (addTEnv te env)) c
                                             y <- newName dummySym
                                             return ([(y,Kindle.TWild)], t, Kindle.cBind [(x,Kindle.Val tx (Kindle.ENew n []))] c)
cFun env (EDo x tx c)                   = do tx <- cAType tx
                                             (t,c) <- cCmd (pushSelf x tx env) c
                                             return ([(x,tx)], t, c)
cFun env e                              = do (t,c) <- cBody env e
                                             return ([], t, c)


-- Convert a Core (Pat,Exp) pair and a scrutinee expression into Kindle.AType and a Kindle.Alt
cAlt cBody env e0 (PLit l, e)           = do (t,c) <- cBody env e
                                             return (t, Kindle.ALit l c)
cAlt cBody env e0 (PCon n, e)           = case findCon env n of
                                            (_,Just te) -> do let e1 = Kindle.ECast (Kindle.TId n) e0
                                                                  (xs,ts) = unzip (tail te)
                                                              (t, c) <- cRhs env e ts (map (Kindle.ESel e1) xs)
                                                              return (t, Kindle.ACon n c)
                                            (_,Nothing) -> do (t,c) <- cBody env e
                                                              return (t, Kindle.ACon n c)
  where cRhs env e [] []                = cBody env e
        cRhs env (ELam te e) ts es      = do te <- cATEnv te
                                             let bs = mkBind te (zipWith3 match (rng te) ts1 es1)
                                             (t,c) <- cRhs (addTEnv' te env) e ts2 es2
                                             return (t, Kindle.cBind bs c)
          where (es1,es2)               = splitAt (length te) es
                (ts1,ts2)               = splitAt (length te) ts
        cRhs env e ts es                = do te <- newEnv paramSym ts
                                             let bs = mkBind te es
                                             (t,c) <- cBody (addTEnv' te env) (EAp e (map eVar (dom te)))
                                             return (t, Kindle.cBind bs c)

-- Convert a Core.Exp into a Kindle.AType and Kindle.Cmd
cBody env (ELet bs e)                   = do (te,bf) <- cBinds env bs
                                             (t,c) <- cBody (addTEnv te env) e
                                             return (t, bf c)
cBody env (ECase e (a:alts) e')         = do (bf,t,e0) <- cExp env e
                                             (t1,a) <- cAlt cBody env e0 a
                                             alts <- mapM (cAltT cBodyT env t1 e0) alts
                                             c <- cBodyT env t1 e'
                                             return (t1, bf (Kindle.CSwitch (scrutinee env t e0) (a:alts) c))
cBody env (EAp (EVar (Prim Refl _) _) [e])      = cBody env e
cBody env (EAp (EVar (Prim Match _) _) [e])     = cBody env e
cBody env (EAp (EVar (Prim Commit _) _) [e])    = cBody env e
cBody env (EVar (Prim Fail _) _)                = return (Kindle.TWild, Kindle.CBreak)
cBody env (EAp (EVar (Prim Fatbar _) _) [e,e']) = do (t,c) <- cBody env e
                                                     c' <- cBodyT env t e'
                                                     return (t, Kindle.CSeq c c')
cBody env e                             = do (bf,t,e) <- cExp env e
                                             return (t, bf (Kindle.CRet e))


-- Convert a Core.Exp into a Kindle.Exp, infer its Kindle.AType and overflow into a list of Kindle.Binds if necessary
cExp env e                              = do (bf,t,h) <- cHead env e
                                             case h of
                                               VHead e -> 
                                                  return (bf, t, e)
                                               FHead f ts -> do
                                                  n <- findClosureName ts t
                                                  te <- newEnv paramSym ts
                                                  let fun = Kindle.Fun t te (Kindle.CRet (f (map Kindle.EVar (dom te))))
                                                  return (bf, Kindle.TId n, Kindle.ENew n [(prim Code, fun)])
                                               

data Head                               = VHead (Kindle.Exp)
                                        | FHead ([Kindle.Exp] -> Kindle.Exp) [Kindle.AType]


-- Convert a Core.Exp into an expression head that is either a value or a function
cHead env (ELit l)                      = return (id, Kindle.litType l, VHead (Kindle.ELit l))
cHead env (ERec c eqs)                  = do (bf,bs) <- cEqs env te eqs
                                             return (bf, Kindle.TId c, VHead (Kindle.ENew c bs))
  where Kindle.Struct te                = lookup' (decls env) c
cHead env (ELet bs e)                   = do (te,bf) <- cBinds env bs
                                             (bf',t,h) <- cHead (addTEnv te env) e
                                             return (bf . bf', t, h)
cHead env (EAp (EVar (Prim Refl _) _) [e])       = cHead env e
cHead env (EAp (EVar (Prim ActToCmd _) _) [e])   = do (bf,t,e) <- cExp env (EAp e defaults)
                                                      return (bf, t, FHead (\_ -> e) [Kindle.TWild])
  where defaults                                 = [EAp (eVar (prim Sec)) [ELit (LInt 0)], eVar (prim Infinity)]
cHead env (EAp (EVar (Prim ReqToCmd _) _) [e])   = cHead env e
cHead env (EAp (EVar (Prim TemplToCmd _) _) [e]) = cHead env e
cHead env (EAp (EVar (Prim RefToPID _) _) [e])   = cHead env e
cHead env (EAp (EVar (Prim Match _) _) [e])      = cHead env e
cHead env (EAp (EVar (Prim Commit _) _) [e])     = cHead env e
cHead env (EAp (EVar (Prim After _) _) [e,e'])   = do (bf,e1) <- cExpT env (Kindle.TId (prim Time)) e
                                                      (bf',t,f,ts) <- cFHead env e'
                                                      return (bf . bf', t, FHead (\[a,b] -> f [sum a e1, b]) ts)
  where sum a e1                                 = Kindle.ECall (prim TimePlus) [a,e1]
cHead env (EAp (EVar (Prim Before _) _) [e,e'])  = do (bf,e1) <- cExpT env (Kindle.TId (prim Time)) e
                                                      (bf',t,f,ts) <- cFHead env e'
                                                      return (bf . bf', t, FHead (\[a,b] -> f [a, min b e1]) ts)
  where min b e1                                 = Kindle.ECall (prim TimeMin) [b,e1]
cHead env (EAp e es)                    = do (bf,t,f,ts) <- cFHead env e
                                             (bf',t',h) <- appFun t f ts es
                                             return (bf . bf', t', h)
  where appFun t f ts es
          | l_ts <  l_es                = error "Internal: c2k.appFun"
          | l_ts == l_es                = do (bf,es) <- cExpTs env ts es
                                             return (bf, t, VHead (f es))
          | l_ts >  l_es                = do (bf,es) <- cExpTs env ts1 es
                                             return (bf, t, FHead (\es' -> f (es++es')) ts2)
          where l_ts                    = length ts
                l_es                    = length es
                (ts1,ts2)               = splitAt (l_es) ts
cHead env (EVar x t')
  | stateVar (annot x) && haveSelf env  = case lookup' (tenv env) x of
                                             Kindle.FunT ts t -> error "Internal: state variable is an FHead in c2k.cHead"
                                             Kindle.ValT t    -> adjust t' id t (VHead (Kindle.ESel (Kindle.EVar (self env)) x))
  | otherwise                           = case lookup' (tenv env) x of
                                             Kindle.FunT ts t -> adjust t' id t (FHead (Kindle.ECall x) ts)
                                             Kindle.ValT t    -> adjust t' id t (VHead (Kindle.EVar x))
cHead env (ESel e l t')                 = do (bf,e) <- cExpT env t1 e
                                             case t2 of
                                               Kindle.FunT ts t -> adjust t' bf t (FHead (Kindle.EEnter e l) ts)
                                               Kindle.ValT t    -> adjust t' bf t (VHead (Kindle.ESel e l))
  where (t1,t2)                         = findSel env l
cHead env (ECon k t')                   = case t2 of
                                             Just te 
                                               | null te'  -> adjust t' id t1 (VHead (newK []))
                                               | otherwise -> adjust t' id t1 (FHead (Kindle.ECast t1 . newK . mkBind te') (rng te'))
                                               where ((tag,tagT):te') = te
                                                     newK = Kindle.ENew k . ((tag, Kindle.Val tagT (Kindle.EVar k)):)
                                             Nothing -> adjust t' id t1 (VHead (Kindle.EVar k))
  where (t1,t2)                         = findCon env k
cHead env e@(ECase _ _ _)               = do (t,c) <- cBody env e       -- Bottom-up mode!
                                             x <- newName tempSym
                                             return (Kindle.cBind [(x, Kindle.Fun t [] c)], t, VHead (Kindle.ECall x []))
cHead env e                             = do (te,t,c) <- cFun (nullSelf env) e     -- Bottom-up mode!
                                             x <- newName tempSym
                                             return (Kindle.cBind [(x, Kindle.Fun t te c)], t, FHead (Kindle.ECall x) (rng te))


-- Adjust the type of a Head on basis of the leaf identifier's annotated instance type (if any)
adjust Nothing bf t h                   = return (bf, t, h)
adjust (Just t0) bf t h                 = do (ts',t') <- cType (deQualify' t0)
                                             return (bf, t', adjust' t h ts' t')
  where adjust' t (VHead e) [] t'       = VHead (match t' t e)
        adjust' t (FHead f ts) ts' t'   = FHead (match t' t . f . zipWith3 match ts ts') ts'


-- Convert a Core.Exp into an expression head that is expected to be a function
cFHead env e                            = do (bf,t,h) <- cHead env e
                                             case h of
                                                FHead f ts -> return (bf, t, f, ts)
                                                VHead e -> do (ts,t') <- findClosureDef n
                                                              return (bf, t', Kindle.EEnter e (prim Code), ts)
                                                  where Kindle.TId n = t


-- Convert a Core.Cmd into a Kindle.AType and a Kindle.Cmd 
cCmd env (CRet e)                       = do (bf,t,e) <- cExp env e
                                             return (t, bindOrphans env bf e (Kindle.CRet e))
cCmd env (CAss x e c)                   = do (bf,e) <- cExpT env tx e
                                             (t,c) <- cCmd env c
                                             return (t, bindOrphans env bf e (Kindle.CAssign (Kindle.EVar (self env)) x e c))
  where tx                              = Kindle.rngType (lookup' (tenv env) x)
cCmd env (CLet bs c)                    = do (te,bf) <- cBinds env bs
                                             (t,c) <- cCmd (addTEnv te env) c
                                             return (t, bindOrphans' env bf c)
cCmd env (CGen x tx e c)
  | isDummy x                           = do (bf,t,e) <- cExp env (EAp e [eVar (self env)])
                                             (t',c) <- cCmd env c
                                             return (t', bindOrphans env bf e (Kindle.CRun e c))
  | otherwise                           = do tx <- cAType tx
                                             (bf,e) <- cExpT env tx (EAp e [eVar (self env)])
                                             (t,c) <- cCmd (addTEnv [(x,Kindle.ValT tx)] env) c
                                             return (t, bindOrphans env bf e (Kindle.cBind [(x,Kindle.Val tx e)] c))
cCmd env (CExp e)                       = cCmdExp env e


-- Convert a Core.Exp in the monadic execution path into a Kindle.AType and a Kindle.Cmd
cCmdExp env (EDo x tx c)                = do tx <- cAType tx
                                             (t,c) <- cCmd (pushSelf x tx env) c
                                             return (t, Kindle.cBind [(x,Kindle.Val tx (Kindle.EVar (self env)))] c)
cCmdExp env (ECase e (a:alts) e')       = do (bf,t,e0) <- cExp env e
                                             (t1,a) <- cAlt cCmdExp env e0 a
                                             alts <- mapM (cAltT cCmdExpT env t1 e0) alts
                                             c <- cBodyT env t1 e'
                                             return (t1, bindOrphans env bf e0 (Kindle.CSwitch (scrutinee env t e0) (a:alts) c))
cCmdExp env e                           = do (bf,t,e) <- cExp env (EAp e [eVar (self env)])
                                             return (t, bindOrphans env bf e (Kindle.CRet e))
