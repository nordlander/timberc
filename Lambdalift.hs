module Lambdalift(lambdalift) where

import Monad
import Common
import Kindle


lambdalift m                            = localStore (llModule m)


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

addExpansions fve env                   = env { expansions = fve ++ expansions env }


-- Convert a module
llModule (Module m ds bs)               = do bs <- mapM (llBind env0) bs
                                             s <- currentStore
                                             let (ds',bs') = splitStore [] [] s
                                             return (Module m (ds++ds') (bs++bs'))
  where env0                            = addDecls ds nullEnv
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
                                             funs' <- mapM (llBind env') funs
                                             mapM_ liftFun funs'
                                             c' <- llCmd env2 c
                                             return (cBindR r vals' c')
  where (vals,funs)                     = partition isVal bs
        free0                           = evars funs
        free1                           = free0 ++ concat [ vs | (f,vs) <- expansions env, f `elem` free0 ]
        fte                             = locals (if r then env1 else env) `restrict` free1
        env1                            = addLocals (mapSnd typeOf' vals) env
        env2                            = addExpansions (dom funs `zip` repeat (dom fte)) env2
        env'                            = if r then env2 else env
        liftFun (x, Fun t te c)         = addToStore (Right (x, Fun t (fte++te) c))
llCmd env (CRet e)                      = liftM CRet (llExp env e)
llCmd env (CRun e c)                    = liftM2 CRun (llExp env e) (llCmd env c)
llCmd env (CAssign e x e' c)            = do e <- llExp env e
                                             liftM2 (CAssign e x) (llExp env e') (llCmd env c)
llCmd env (CSwitch e alts d)            = liftM3 CSwitch (llExp env e) (mapM (llAlt env) alts) (llCmd env d)
llCmd env (CSeq c c')                   = liftM2 CSeq (llCmd env c) (llCmd env c')
llCmd env (CBreak)                      = return CBreak


-- Convert a switch alternative
llAlt env (ACon x c)                    = liftM (ACon x) (llCmd env c)
llAlt env (ALit l c)                    = liftM (ALit l) (llCmd env c)


-- Convert an expression
llExp env (ECall x es)                  = do es <- mapM (llExp env) es
                                             case lookup x (expansions env) of
                                                Just vs -> return (ECall x (map EVar vs ++ es))
                                                Nothing -> return (ECall x es)
llExp env (ENew n bs)
  | null fte                            = liftM (ENew n) (mapM (llBind env) bs)
  | otherwise                           = do n' <- newName (str n)
                                             addToStore (Left (n', Kindle.Struct (te ++ mapSnd ValT fte)))
                                             vals' <- mapM (llBind env) vals
                                             funs' <- mapM (llBind (setThisVars (dom fte) env)) funs
                                             return (ECast (TId n) (ENew n' (vals' ++ funs' ++ map close fte)))
  where (vals,funs)                     = partition isVal bs
        free0                           = evars funs
        free1                           = free0 ++ concat [ vs | (f,vs) <- expansions env, f `elem` free0 ]
        fte                             = locals env `restrict` free1
        Kindle.Struct te                = lookup' (decls env) n
        close (x,t)                     = (x, Val t (EVar x))
llExp env (EVar x)
  | x `elem` thisVars env               = return (ESel EThis x)
  | otherwise                           = return (EVar x)
llExp env (EThis)                       = return (EThis)
llExp env (ELit l)                      = return (ELit l)
llExp env (ESel e l)                    = liftM (flip ESel l) (llExp env e)
llExp env (EEnter e f es)               = do e <- llExp env e
                                             liftM (EEnter e f) (mapM (llExp env) es)
llExp env (ECast t e)                   = liftM (ECast t) (llExp env e)


{-llBody env (BDef fs b)                  = tr' ("Lifting  " ++ showids xs ++ "\n" ++
                                               " free0:  " ++ showids free0 ++ "\n" ++
                                               " free1:  " ++ showids free1 ++ "\n" ++
                                               " locals: " ++ showids (locals env) ++ "\n" ++
                                               " fte:    " ++ show fte ++ "\n" ++
                                               " fs':    " ++ show fs' ++ "\n" ++
                                               " lifted: " ++ show (map lift fs1) ++ "\n") 
                                              (concat fss ++ map lift fs' ++ fs'', b')
  where xs                              = dom fs
        te                              = funEnv fs
        free0                           = evars fs \\ xs
        free1                           = free0 ++ concat [ ys | (y,ys) <- expansions env, y `elem` free0 ]
        fte                             = locals env `restrict` free1
        env'                            = addExpansions env (xs `zip` repeat (dom fte))
        (fss,fs')                       = unzip (map (llFDef env') fs)
        lift (x, Fun te t b)            = (x, Fun (fte++te) t b)
        (fs'', b')                      = llBody env' b
-}

