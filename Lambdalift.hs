module Lambdalift(lambdalift) where

import Monad
import Common
import Kindle


lambdalift ds m                     = localStore (llModule ds m)


data Env                                = Env { decls      :: Decls,                -- global type declarations
                                                thisVars   :: [Name],               -- variables reachable through "this"
                                                locals     :: ATEnv,                -- non-global values in scope
                                                expansions :: Map Name [Name]       -- non-global functions and their added parameters
                                              }

nullEnv                                 = Env { decls = primDecls, 
                                                thisVars = [],
                                                locals = [],
                                                expansions = []
                                              }

addDecls ds env                         = env { decls = ds ++ decls env }

setThisVars vs env                      = env { thisVars = vs }

addLocals te env                        = env { locals = te ++ locals env }

addExpansions exps env                  = env { expansions = exps ++ expansions env }


-- Convert a module
llModule dsi (Module m ns ds bs)        = do bs <- mapM (llBind env0) bs
                                             s <- currentStore
                                             let (ds',bs') = splitStore [] [] s
                                             return (Module m ns (ds++ds') (bs++bs'))
  where env0                            = addDecls (ds ++ dsi) nullEnv
        splitStore ds bs []             = (ds, bs)
        splitStore ds bs (Left d : s)   = splitStore (d:ds) bs s
        splitStore ds bs (Right b : s)  = splitStore ds (b:bs) s


-- Convert a binding
llBind env (x, Fun t te c)              = do c <- llCmd (addLocals te env) c
                                             return (x, Fun t te c)
llBind env (x, Val t e)                 = do e <- llExp env e
                                             return (x, Val t e)


{- Lambda-lifting a set of recursive bindings:

   \v ->                                                letrec f r v x = ... r ... v ...
   letrec f x = ... r ... v ...                                g r v y = ... f r v e ...
          r   = { a = ... g e ... }         ==>         in \v ->
          g y = ... f e ...                             letrec r  = { a = ... g r v e ... }
   in f e                                               in f r v e
-}

{- Closing a struct with function fields:

   \v ->                                                \v ->
   { f x = ... v ...                                    { f x = ... this.v ...
     w   = ... v ... }                      ==>           w   = ... v ...
                                                          v   = v }
-}

-- Convert a command
llCmd env (CBind r bs c)                = do vals' <- mapM (llBind env') vals
                                             funs' <- mapM (llBind (setThisVars [] env')) funs
                                             mapM_ liftFun funs'
                                             c' <- llCmd env2 c
                                             return (cBindR r vals' c')
  where (vals,funs)                     = partition isVal bs
        free0                           = evars funs
        free1                           = free0 ++ concat [ xs | (f,xs) <- expansions env, f `elem` free0 ]
        fte                             = locals (if r then env1 else env) `restrict` free1
        env1                            = addLocals (mapSnd typeOf' vals) env
        env2                            = addExpansions (dom funs `zip` repeat (dom fte)) env1
        env'                            = if r then env2 else env
        liftFun (x, Fun t te c)         = addToStore (Right (x, Fun t (fte++te) c))
llCmd env (CRet e)                      = liftM CRet (llExp env e)
llCmd env (CRun e c)                    = liftM2 CRun (llExp env e) (llCmd env c)
llCmd env (CUpd x e c)                  = liftM2 (CUpd x) (llExp env e) (llCmd env c)
llCmd env (CUpdS e x e' c)              = do e <- llExp env e
                                             liftM2 (CUpdS e x) (llExp env e') (llCmd env c)
llCmd env (CUpdA e i e' c)              = liftM4 CUpdA (llExp env e) (llExp env i) (llExp env e') (llCmd env c)
llCmd env (CSwitch e alts)              = liftM2 CSwitch (llExp env e) (mapM (llAlt env) alts)
llCmd env (CSeq c c')                   = liftM2 CSeq (llCmd env c) (llCmd env c')
llCmd env (CBreak)                      = return CBreak
llCmd env (CRaise e)                    = liftM CRaise (llExp env e)
llCmd env (CWhile e c c')               = liftM3 CWhile (llExp env e) (llCmd env c) (llCmd env c')
llCmd env (CCont)                       = return CCont


-- Convert a switch alternative
llAlt env (ACon x c)                    = liftM (ACon x) (llCmd env c)
llAlt env (ALit l c)                    = liftM (ALit l) (llCmd env c)
llAlt env (AWild c)                     = liftM AWild (llCmd env c)


-- Convert an expression
llExp env (ECall x es)                  = do es <- mapM (llExp env) es
                                             case lookup x (expansions env) of
                                                Just xs -> return (ECall x (map (mkEVar env) xs ++ es))
                                                Nothing -> return (ECall x es)
llExp env (ENew n bs)
  | null fte                            = liftM (ENew n) (mapM (llBind env) bs)
  | otherwise                           = do n' <- newName typeSym
                                             addToStore (Left (n', Kindle.Struct (te ++ mapSnd ValT fte) []))
                                             vals' <- mapM (llBind env) vals
                                             funs' <- mapM (llBind (setThisVars (dom fte) env)) funs
                                             return (ECast (TId n) (ENew n' (vals' ++ funs' ++ map close fte)))
  where (vals,funs)                     = partition isVal bs
        free0                           = evars funs
        free1                           = free0 ++ concat [ xs | (f,xs) <- expansions env, f `elem` free0 ]
        fte                             = locals env `restrict` free1
        Kindle.Struct te _              = lookup' (decls env) n
        close (x,t)                     = (x, Val t (mkEVar env x))
llExp env (EVar x)                      = return (mkEVar env x)
llExp env (EThis)                       = return (EThis)
llExp env (ELit l)                      = return (ELit l)
llExp env (ESel e l)                    = liftM (flip ESel l) (llExp env e)
llExp env (EEnter e f es)               = do e <- llExp env e
                                             liftM (EEnter e f) (mapM (llExp env) es)
llExp env (ECast t e)                   = liftM (ECast t) (llExp env e)


mkEVar env x                            = if x `elem` thisVars env then ESel EThis x else EVar x
