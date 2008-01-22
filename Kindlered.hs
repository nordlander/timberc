module Kindlered where
    
import Monad
import Common
import Kindle
import PP


kindlered ds m                          = redModule ds m


data Env                                = Env { decls :: Decls }

nullEnv                                 = Env { decls = [] }

addDecls ds env                         = env { decls = ds ++ decls env }

findSel env l                           = head [ t | (_,Struct te _) <- decls env, (l',t) <- te, l'==l ]


-- Convert a module
redModule dsi (Module m ns ds bs)       = do bs <- mapM (redBind env0) bs
                                             return (Module m ns ds bs)
  where env0                            = addDecls (ds++dsi) nullEnv


-- Convert a binding
redBind env (x, Fun t te c)             = do c <- redCmd env c
                                             return (x, Fun t te c)
redBind env (x, Val t e)                = do e <- redExp env e
                                             return (x, Val t e)


single x e                              = length (filter (==x) (evars e)) == 1

newObj (ENew _ bs)                      = prim Obj `elem` dom bs
newObj _                                = False

-- Convert a command
redCmd env (CRet e)                     = do e <- redExp env e
                                             redRet env e
redCmd env e0@(CBind False [(x,Val _ e)] (CRet e'))
  | single x e' && not (newObj e)       = redCmd env (CRet (subst [(x,e)] e'))
redCmd env (CBind r bs c)               = liftM2 (CBind r) (mapM (redBind env) bs) (redCmd env c)
redCmd env (CRun e c)                   = liftM2 CRun (redExp env e) (redCmd env c)
redCmd env (CUpd x e c)                 = liftM2 (CUpd x) (redExp env e) (redCmd env c)
redCmd env (CUpdS e x v c)              = do e <- redExp env e
                                             v <- redExp env v
                                             c <- redCmd env c
                                             redUpdArray0 env e x v c (arrayDepth t)
  where ValT t                          = findSel env x
redCmd env (CUpdA e i e' c)             = do e <- redExp env e
                                             liftM2 (CUpdA e i) (redExp env e') (redCmd env c)
redCmd env (CSwitch e alts)             = liftM2 CSwitch (redExp env e) (mapM (redAlt env) alts)
redCmd env (CSeq c c')                  = liftM2 CSeq (redCmd env c) (redCmd env c')
redCmd env (CBreak)                     = return CBreak
redCmd env (CRaise e)                   = liftM CRaise (redExp env e)
redCmd env (CWhile e c c')              = liftM3 CWhile (redExp env e) (redCmd env c) (redCmd env c')
redCmd env (CCont)                      = return CCont


redRet env (EEnter e f es)              = do c <- redRet env e
                                             return (cMap (ff env f es) c)
redRet env e                            = return (CRet e)


ff env f es (ENew n bs)                 = subst (dom te `zip` es) c
  where Fun t te c                      = lookup' bs f
ff env f es e                           = CRet (EEnter e f es)



-- Convert a switch alternative
redAlt env (ACon x c)                   = liftM (ACon x) (redCmd env c)
redAlt env (ALit l c)                   = liftM (ALit l) (redCmd env c)
redAlt env (AWild c)                    = liftM AWild (redCmd env c)




-- Convert an expression
redExp env (EEnter e f es)              = do e <- redExp env e
                                             es <- mapM (redExp env) es
                                             redEnter env e f es
redExp env e@(ECall (Prim IndexArray _) _)
                                        = redIndexArray env e []
redExp env (ECall x es)                 = do es <- mapM (redExp env) es
                                             return (ECall x es)
redExp env (ENew n bs)                  = liftM (ENew n) (mapM (redBind env) bs)
redExp env (EVar x)                     = return (EVar x)
redExp env (EThis)                      = return (EThis)
redExp env (ELit l)                     = return (ELit l)
redExp env a@(ESel e l)
  | stateVar (annot l)                  = redIndexArray env a []
  | otherwise                           = liftM (flip ESel l) (redExp env e)
redExp env (ECast t e)                  = liftM (ECast t) (redExp env e)


redEnter env e@(ENew n bs) f es         = case c of
                                             CRet e' -> return (subst (dom te `zip` es) e')
                                             _       -> return (EEnter e f es)
  where Fun t te c                      = lookup' bs f
redEnter env e f es                     = return (EEnter e f es)


-- Convert an array indexing expression
redIndexArray env (ECall (Prim IndexArray _) [a,i]) is
                                    = redIndexArray env a (i:is)
redIndexArray env a@(ESel e l) is
  | stateVar (annot l)              = do e <- mkIndex env a is
                                         return (clone (arrayDepth t - length is) e)
  where ValT t                      = findSel env l
redIndexArray env a is              = do a <- redExp env a
                                         e <- mkIndex env a is
                                         return e


mkIndex env a is                    = do is <- mapM (redExp env) is
                                         return (foldl f a is)
  where f a i                       = ECall (prim IndexArray) [a,i]
                                         

arrayDepth (TArray t)               = 1 + arrayDepth t
arrayDepth _                        = 0


clone 0 e                           = e
clone n e                           = ECall (prim CloneArray) [ELit (LInt Nothing (toInteger n)), e]


{-
    a|x|y|z := e
    a|x|y   := a|x|y \\ (z,e)
    a|x     := a|x \\ (y, a|x|y \\ (z,e))
    a       := a \\ (x, a|x \\ (y, a|x|y \\ (z,e)))
-}

redUpdArray0 env e x (ECall (Prim UpdateArray _) [a,i,v]) c n
  | a == ESel e x                   = redUpdArray1 env a i v c (n-1)
  where 
redUpdArray0 env e x v c n          = return (CUpdS e x (clone n v) c)


redUpdArray1 env e i (ECall (Prim UpdateArray _) [a,i',v]) c n
  | a == e'                         = redUpdArray1 env a i' v c (n-1)
  where e'                          = ECall (prim IndexArray) [e,i]
redUpdArray1 env e i v c n          = return (CUpdA e i (clone n v) c)

