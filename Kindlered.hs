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
redCmd env (CUpdS e x v c)              = do -- tr ("redAssign " ++ show (arrayDepth t) ++ " " ++ render (pr v) ++ " :: " ++ render (pr t))
                                             f <- redAssign env (arrayDepth t) (ESel e x) id v
                                             liftM f (redCmd env c)
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
                                        = redIndexArray env e 0
redExp env (ECall p@(Prim SizeArray _) [e])
                                        = liftM (ECall p . (:[])) (redExp' env e)
redExp env (ECall x es)                 = do es <- mapM (redExp env) es
                                             return (ECall x es)
redExp env (ENew n bs)                  = liftM (ENew n) (mapM (redBind env) bs)
redExp env (EVar x)                     = return (EVar x)
redExp env (EThis)                      = return (EThis)
redExp env (ELit l)                     = return (ELit l)
redExp env a@(ESel e l)
  | stateVar (annot l)                  = redIndexArray env a 0
  | otherwise                           = liftM (flip ESel l) (redExp env e)
redExp env (ECast t e)                  = liftM (ECast t) (redExp env e)


redEnter env e@(ENew n bs) f es         = case c of
                                             CRet e' -> return (subst (dom te `zip` es) e')
                                             _       -> return (EEnter e f es)
  where Fun t te c                      = lookup' bs f
redEnter env e f es                     = return (EEnter e f es)


-- Convert an expression in a safe context (no cloning needed)
redExp' env (ECast t e)                 = liftM (ECast t) (redExp' env e)
redExp' env (ESel e l)                  = liftM (flip ESel l) (redExp env e)
redExp' env e                           = redExp env e


-- Convert an array indexing expression
redIndexArray env (ECall (Prim IndexArray _) [a,i]) n
                                    = do a <- redIndexArray env a (n+1)
                                         i <- redExp env i
                                         return (indexArray a i)
redIndexArray env (ECast t e) n     = fmap (ECast t) (redIndexArray env e n)
redIndexArray env (ESel e l) n
  | stateVar (annot l)              = do e <- redExp env e
                                         return (clone (arrayDepth t - n) id (ESel e l))
  where ValT t                      = findSel env l
redIndexArray env a n               = redExp env a


indexArray a i                      = ECall (prim IndexArray) [a,i]


intExp i                            = ELit (LInt Nothing (toInteger i))

arrayDepth (TArray t)               = 1 + arrayDepth t
arrayDepth _                        = 0


clone n g (ECast t e)               = clone n (g . ECast t) e
clone 0 g e                         = g e
clone 1 g e@(ECall (Prim ListArray _) _)
                                    = g e
clone 1 g e@(ECall (Prim UniArray _) _)
                                    = g e
clone _ g e@(ECall (Prim EmptyArray _) _)
                                    = g e
clone _ g e@(ECall (Prim SizeArray _) _)
                                    = g e
clone n g (ECall (Prim CloneArray _) [e,ELit (LInt _ n')])
  | toInteger n == n'               = g e
clone n g e                         = g (ECall (prim CloneArray) [ECast (TArray TWild) e, intExp n])


{-
    a|x|y|z := e
    a|x|y   := a|x|y \\ (z,e)
    a|x     := a|x \\ (y, a|x|y \\ (z,e))
    a       := a \\ (x, a|x \\ (y, a|x|y \\ (z,e)))
-}

redAssign env n e0 g (ECast t e)    = redAssign env n e0 (g . ECast t) e
redAssign env n e0 g (ECall (Prim UpdateArray _) [a,i,v])
  | e0 == a || ECast (TArray TWild) e0 == a
                                    = redAssign env (n-1) (indexArray a i) id v
  | otherwise                       = do f1 <- redAssign env n e0 g a
                                         f2 <- redAssign env (n-1) (indexArray e0 i) id v
                                         return (f1 . f2)
redAssign env n e0 g (ECall (Prim ListArray _) [e])
  | Just es <- constElems e, 
    let m = length es               = do f <- redAssign env n e0 g (ECall (prim EmptyArray) [ELit (LInt Nothing (toInteger m))])
                                         fs <- mapM mkAssign ([0..] `zip` es)
                                         return (foldl (.) f fs)
  where constElems (ECast _ (ENew (Prim CONS _) bs))
                                    = do es <- constElems eb
                                         return (ea:es)
          where Val _ ea            = lookup' bs (head abcSupply)
                Val _ eb            = lookup' bs (head (tail abcSupply))
        constElems (ECast _ (ENew (Prim NIL _) bs))
                                    = Just []
        constElems _                = Nothing
        mkAssign (i,e)              = redAssign env (n-1) (indexArray e0 (intExp i)) id e
redAssign env n e0 g (ECall (Prim UniArray _) [m,e])
                                    = do x <- newName tempSym
                                         s <- newName tempSym
                                         i <- newName paramSym
                                         let f0 = cBind [(x,Val TWild e),(s,Val tInt m),(i,Val tInt (intExp 0))]
                                         f <- redAssign env n e0 g (ECall (prim EmptyArray) [EVar s])
                                         let f1 c = CWhile (ECall (prim IntLT) [EVar i, EVar s]) c
                                         f2 <- redAssign env (n-1) (indexArray e0 (EVar i)) id (EVar x)
                                         let c = CUpd i (ECall (prim IntPlus) [EVar i, intExp 1]) CCont
                                         return (f0 . f . f1 (f2 c))
redAssign env n (ESel e x) g v      = do v' <- redExp env v
                                         return (CUpdS e x (clone n g v'))
redAssign env n (ECall (Prim IndexArray _) [e,i]) g v
                                    = do v' <- redExp env v
                                         return (CUpdA e i (clone n g v'))


