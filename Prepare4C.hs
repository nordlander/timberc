module Prepare4C where

import Monad
import Common
import Kindle


-- Establishes that:
--   every struct function simply relays all work to a global function with an explicit "this" parameter
--   every EEnter expression has a variable as its body
--   every ENew expression occurs at the rhs of a Val binding

-- (Not yet):
-- Flattens the struct subtyping graph by inlining the first coercion field
-- Ditto for enums...
-- Replaces tag-only structs with casts from the actual tag value (and corresponding switch cmds)


prepare4c m                     = localStore (pModule m)


data Env                        = Env { decls :: Decls,
                                        tenv  :: TEnv,
                                        this  :: Maybe (Name,AType) }

env0                            = Env { decls = primDecls, tenv = primTEnv, this = Nothing }

addDecls ds env                 = env { decls = ds ++ decls env }

addBinds bs env                 = env { tenv = mapSnd typeOf bs ++ tenv env }

addVals te env                  = env { tenv = mapSnd ValT te ++ tenv env }

setThis x t env                 = env { this = Just (x,t) }

pModule (Module m ds bs)        = do ds <- mapM pDecl ds
                                     (bs1,bs) <- pMap (pBind (addBinds bs (addDecls ds env0))) bs
                                     bs2 <- currentStore
                                     return (Module m ds (bs1++bs++bs2))


pDecl d                         = return d


pMap f xs                       = do (bss,xs) <- fmap unzip (mapM f xs)
                                     return (concat bss, xs)

-- Prepare bindings
pBind env (x, Val t e)          = do (bs,e) <- pRhsExp env e 
                                     return (bs, (x, Val t e))
pBind env (x, Fun t te c)       = do c <- pCmd (addVals te env) c
                                     return ([], (x, Fun t te c))


-- Prepare struct bindings
pSBind env n (x, Val t e)       = do (bs,e) <- pRhsExp env e 
                                     return (bs, (x, Val t e))
pSBind env n b@(x, Fun t te (CRet (ECall f (EThis:es))))
  | es == map EVar (dom te)     = return ([], b)
pSBind env n (x, Fun t te c)    = do f <- newName functionSym
                                     y <- newName tempSym
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
                                     liftM (cBind (bs ++ bs') . CAssign e x e') (pCmd env c)
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
                                     return (bs1, ENew n bs)
pRhsExp env e                   = pExp env e


-- Prepare an expression in an arbitrary position
pExp env e@(EVar _)             = return ([], e)
pExp env e@(ELit _)             = return ([], e)
pExp env (EThis)                = return ([], EVar (fst (fromJust (this env))))
pExp env (ESel e l)             = do (bs,e) <- pExp env e
                                     return (bs, ESel e l)
pExp env (ECall n es)           = do (bs,es) <- pMap (pExp env) es
                                     return (bs, ECall n es)
pExp env (EEnter (EVar x) n es) = do (bs,es) <- pMap (pExp env) es
                                     return (bs, EEnter (EVar x) n es)
pExp env (EEnter e n es)        = do (bs1,es) <- pMap (pExp env) es
                                     (bs2,e) <- pRhsExp env e
                                     x <- newName tempSym
                                     return (bs1++bs2++[(x, Val t e)], EEnter (EVar x) n es)
  where t                       = typeOfExp env e
pExp env (ECast t e)            = do (bs,e) <- pExp env e
                                     return (bs, ECast t e)
pExp env (ENew n bs)            = do (bs1,bs) <- pMap (pSBind env n) bs
                                     x <- newName tempSym
                                     return (bs1++[(x, Val (TId n) (ENew n bs))], EVar x)

                                     

-- Compute the type of an expression
typeOfExp env (EVar x)          = case searchCons (decls env) x of
                                     [t] -> t
                                     _   -> rngType (lookup' (tenv env) x)
typeOfExp env (ELit l)          = litType l
typeOfExp env (EThis)           = snd (fromJust (this env))
typeOfExp env (ESel e x)        = rngType (lookup' te x)
  where TId n                   = typeOfExp env e
        Struct te               = lookup' (decls env) n
typeOfExp env (ECall x es)      = rngType (lookup' (tenv env) x)
typeOfExp env (EEnter e x es)   = rngType (lookup' te x)
  where TId n                   = typeOfExp env e
        Struct te               = lookup' (decls env) n
typeOfExp env (ECast t e)       = t
