module Prepare4C(prepare4c) where

import Monad
import Common
import Kindle


-- Establishes that:
--   every struct function simply relays all work to a global function with an explicit "this" parameter
--   every EEnter expression has a variable as its body
--   every ENew expression occurs at the rhs of a Val binding

-- Implements boxing and unboxing of non-ptr values that are cast to/from polymorphic type

-- (Not yet):
-- Flattens the struct subtyping graph by inlining the first coercion field
-- Ditto for enums...
-- Replaces tag-only structs with casts from the actual tag value (and corresponding switch cmds)


prepare4c envi m                = localStore (pModule envi m)


data Env                        = Env { decls :: Decls,
                                        tenv  :: TEnv,
                                        this  :: Maybe (Name,AType) }

env0                            = Env { decls = primDecls, tenv = primTEnv, this = Nothing }

addDecls ds env                 = env { decls = ds ++ decls env }

addBinds bs env                 = env { tenv = mapSnd typeOf bs ++ tenv env }

addVals te env                  = env { tenv = mapSnd ValT te ++ tenv env }

setThis x t env                 = env { this = Just (x,t) }

findDecl env n
  | isTuple n                   = Struct (mapSnd ValT (tupleSels n))
  | otherwise                   = lookup' (decls env) n


pModule (dsi,tei,_) (Module m ds bs)      
                                = do ds <- mapM pDecl ds
                                     (bs1,bs) <- pMap (pBind (addBinds bs (addDecls ds env))) bs
                                     bs2 <- currentStore
                                     return (Module m ds (bs1 ++ bs ++ reverse bs2))
  where env                     = addDecls dsi (env0 {tenv = tei ++ tenv env0})


pDecl d                         = return d


pMap f xs                       = do (bss,xs) <- fmap unzip (mapM f xs)
                                     return (concat bss, xs)


-- Prepare bindings
pBind env (x, Val t e)          = do (bs,_,e) <- pRhsExp env e 
                                     return (bs, (x, Val t e))
pBind env (x, Fun t te c)       = do c <- pCmd (addVals te env) c
                                     return ([], (x, Fun t te c))


-- Prepare struct bindings
pSBind env n (x, Val t e)       = do (bs,_,e) <- pRhsExp env e 
                                     return (bs, (x, Val t e))
pSBind env n b@(x, Fun t te (CRet (ECall f (EThis:es))))
  | es == map EVar (dom te)     = return ([], b)
pSBind env n (x, Fun t te c)    = do f <- newName functionSym
                                     y <- newName thisSym
                                     c <- pCmd (addVals te (setThis y (TId n) env)) c
                                     addToStore (f, Fun t ((y,TId n):te) c)
                                     return ([], (x, Fun t te (CRet (ECall f (EThis : map EVar (dom te))))))


-- Prepare commands
pCmd env (CRet e)               = do (bs,e) <- pExp env e
                                     return (cBind bs (CRet e))
pCmd env (CRun e c)             = do (bs,e) <- pExp env e
                                     liftM (cBind bs . CRun e) (pCmd env c)
pCmd env (CBind r bs c)         = do (bs1,bs) <- pMap (pBind (if r then env' else env)) bs
                                     liftM (cBind bs1 . CBind r bs) (pCmd env' c)
  where env'                    = addBinds bs env
pCmd env (CAssign e x e' c)     = do (bs,e) <- pExp env e
                                     (bs',e') <- pExp env e'
                                     liftM (cBind bs . cBind bs' . CAssign e x e') (pCmd env c)
pCmd env (CSwitch e alts d)     = do (bs,e) <- pExp env e
                                     alts <- mapM (pAlt env) alts
                                     liftM (cBind bs . CSwitch e alts) (pCmd env d)
pCmd env (CSeq c c')            = liftM2 CSeq (pCmd env c) (pCmd env c')
pCmd env (CBreak)               = return CBreak


-- Prepare switch alternatives
pAlt env (ALit l c)             = liftM (ALit l) (pCmd env c)
pAlt env (ACon n c)             = liftM (ACon n) (pCmd env c)


-- Prepare a right-hand-side expression
pRhsExp env (ENew n bs)         = do (bs1,bs) <- pMap (pSBind env n) bs
                                     return (bs1, TId n, ENew n bs)
pRhsExp env e                   = pExp' env e


-- Prepare an expression in an arbitrary position
pExp env e                      = do (bs,t,e) <- pExp' env e
                                     return (bs,e)
                                     

-- Prepare an expression in an arbitrary position and compute its type
pExp' env e@(EVar x)                = case searchCons (decls env) x of
                                        [t] -> return ([], t, e)
                                        _   -> return ([], rngType (lookup' (tenv env) x), e)
pExp' env e@(ELit l)                = return ([], litType l, e)
pExp' env (EThis)                   = return ([], t, EVar x)
  where (x,t)                       = fromJust (this env)
pExp' env (ESel e l)                = do (bs,TId n,e) <- pExp' env e
                                         let Struct te = findDecl env n
                                         return (bs, rngType (lookup' te l), ESel e l)
pExp' env (ECall f es)              = do (bs,es) <- pMap (pExp env) es
                                         return (bs, rngType (lookup' (tenv env) f), ECall f es)
pExp' env (EEnter (EVar x) f es)    = do (bs1,TId n,e) <- pExp' env (EVar x)
                                         (bs2,es) <- pMap (pExp env) es
                                         let Struct te = findDecl env n
                                         return (bs1++bs2, rngType (lookup' te f), EEnter e f es)
pExp' env (EEnter e f es)           = do (bs1,t@(TId n),e) <- pRhsExp env e
                                         (bs2,es) <- pMap (pExp env) es
                                         x <- newName tempSym
                                         let Struct te = findDecl env n
                                         return (bs1++bs2++[(x, Val t e)], rngType (lookup' te f), EEnter (EVar x) f es)
pExp' env (ECast TWild e)           = do (bs,t',e) <- pExp' env e
                                         if mustBox env t' then do
                                             x <- newName tempSym
                                             let bs1 = [(x, Val (TId (prim Box)) (ENew (prim Box) bs2))]
                                                 bs2 = [(prim Value, Val TWild (ECast TWild e))]
                                             return (bs++bs1, TWild, ECast TWild (EVar x))
                                          else
                                             return (bs, TWild, ECast TWild e)
pExp' env (ECast t e)               = do (bs,t',e) <- pExp' env e
                                         if t'==TWild && mustBox env t then
                                             return (bs, t, ECast t (ESel (ECast (TId (prim Box)) e) (prim Value)))
                                          else
                                             return (bs, t, ECast t e)
pExp' env (ENew n bs)               = do (bs1,bs) <- pMap (pSBind env n) bs
                                         x <- newName tempSym
                                         return (bs1++[(x, Val (TId n) (ENew n bs))], TId n, EVar x)


mustBox env TWild                   = False                             -- Already polymorphic
mustBox env (TId n)
  | isTuple n                       = False
  | otherwise                       = case lookup n (decls env) of
                                         Just (Struct te) -> False      -- Already heap allocated
                                         Just (Enum cs)   -> False      -- Limited range, can't be confused with a pointer
                                         _ -> True                      -- In effect, integers and floats
