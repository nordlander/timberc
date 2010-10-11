{-# LANGUAGE PatternGuards, ParallelListComp, FlexibleInstances #-}

-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Kindlered where

import Control.Monad
import Common
import Kindle
import PP

kindlered ds m                          = redModule ds m


data Env                                = Env { decls :: Decls }

nullEnv                                 = Env { decls = [] }

addDecls ds env                         = env { decls = ds ++ decls env }

findSel env l                           = head [ t | (_,Struct _ te _) <- decls env, (l',t) <- te, l'==l ]


-- Convert a module
redModule dsi (Module m ns es ds bs)    = do bs <- mapM (redBind env0) bs
                                             return (Module m ns es ds bs)
  where env0                            = addDecls (ds++dsi) nullEnv

-- Convert a binding
redBind env (f, Fun vs t te c)          = do c <- redCmd env c
                                             c <- tailOptimize f t te c
                                             return (f, Fun vs t te c)
redBind env (x, Val t e)                = do e <- redExp env e
					     e' <- tailOptimizeVal x e
                                             return (x, Val t e')

-- Tail recursion optimization

tailOptimize f (TCon (Prim LIST _) [t]) te c
  | isTailRecModCONS isTailCall f c     = do x <- newName tempSym
                                             c <- redTailCallModCONS isTailCall doTailCall f x t te c
                                             let c' = case c of
                                                        CRet e -> c
                                                        _ -> CWhile (ELit (lInt 1)) c (CRaise (ELit (lInt 1)))
                                             return (cBind [(x, Val (tMUTLIST t) (ECall (prim MUTLISTINIT) [t] []))] c')
tailOptimize f t te c
  | isTailRecursive isTailCall f c      = do c <- redTailCall isTailCall doTailCall f te c
                                             return (CWhile (ELit (lInt 1)) c (CRaise (ELit (lInt 1))))
tailOptimize f t te c                   = return c


tailOptimizeVal f (EClos vs (TCon (Prim LIST _) [t]) te c)
  | isTailRecModCONS isTailEnter f c	= do x <- newName tempSym
					     c <- redTailCallModCONS isTailEnter doTailEnter f x t te c
					     let c' = CWhile (ELit (lInt 1)) c (CRaise (ELit (lInt 1)))
					     return (EClos vs t te (cBind [(x, Val (tMUTLIST t) (ECall (prim MUTLISTINIT) [t] []))] c'))
tailOptimizeVal f (EClos vs t te c)
  | isTailRecursive isTailEnter f c	= do c <- redTailCall isTailEnter doTailEnter f te c
					     return (EClos vs t te (CWhile (ELit (lInt 1)) c (CRaise (ELit (lInt 1)))))
tailOptimizeVal f e			= return e

{-
                                   f (xs) {}
  f (xs) {                             while (1) {
     ...                                   ...
     return f(es)                          xs = es; continue;
     ...                                   ...
     return e                              return e
     ...                                   ...
  }                                    }
                                   }
-}

isTailCall f (ECall g _ _)		= f == g
isTailCall f e				= False

isTailEnter x (EEnter (EVar y) (Prim Code _) _ _)
					= x == y
isTailEnter x e				= False

isTailRecursive isT f (CBind False [(_, Val t e)] (CRet (ENew (Tuple 0 _) _ _)))
 | t == tUNIT                           = isT f e
isTailRecursive isT f (CRet e)  	= isT f e
isTailRecursive isT f (CRun _ c)        = isTailRecursive isT f c
isTailRecursive isT f (CBind _ _ c)     = isTailRecursive isT f c
isTailRecursive isT f (CUpd _ _ c)      = isTailRecursive isT f c
isTailRecursive isT f (CUpdS _ _ _ c)   = isTailRecursive isT f c
isTailRecursive isT f (CUpdA _ _ _ c)   = isTailRecursive isT f c
isTailRecursive isT f (CSwitch _ alts)  = or [isTailRecursiveAlt isT f a | a <- alts]
isTailRecursive isT f (CSeq c1 c2)      = isTailRecursive isT f c1 || isTailRecursive isT f c2
isTailRecursive isT f (CWhile _ c1 c2)  = isTailRecursive isT f c2
isTailRecursive isT _ _                 = False

isTailRecursiveAlt isT f (ACon _ _ _ c) = isTailRecursive isT f c
isTailRecursiveAlt isT f (ALit _ c)     = isTailRecursive isT f c
isTailRecursiveAlt isT f (AWild c)      = isTailRecursive isT f c

doTailCall vs (ECall _ _ es)		= updateParams vs es

doTailEnter vs (EEnter _ _ _ es)	= updateParams vs es


redTailCall isT doT f vs (CBind False [(_, Val t e)] (CRet (ENew (Tuple 0 _) _ _)))
  | t == tUNIT && isT f e        		= doT vs e
redTailCall isT doT f vs (CRet e)
  | isT f e                      		= doT vs e
redTailCall isT doT f vs (CBind r bs c) 	= liftM (CBind r bs) (redTailCall isT doT f vs c)
redTailCall isT doT f vs (CRun e c)     	= liftM (CRun e) (redTailCall isT doT f vs c)
redTailCall isT doT f vs (CUpd y e c)   	= liftM (CUpd y e) (redTailCall isT doT f vs c)
redTailCall isT doT f vs (CUpdS e y v c)        = liftM (CUpdS e y v) (redTailCall isT doT f vs c)
redTailCall isT doT f vs (CUpdA e i e' c)       = liftM (CUpdA e i e') (redTailCall isT doT f vs c)
redTailCall isT doT f vs (CSwitch e alts)       = liftM (CSwitch e) (mapM (redTailAlt isT doT f vs) alts)
redTailCall isT doT f vs (CSeq c c')            = liftM2 CSeq (redTailCall isT doT f vs c) (redTailCall isT doT f vs c')
redTailCall isT doT f vs (CWhile e c c')        = liftM (CWhile e c) (redTailCall isT doT f vs c')
redTailCall isT doT f _ c                       = return c

redTailAlt isT doT f vs (ACon y us te c)        = liftM (ACon y us te) (redTailCall isT doT f vs c)
redTailAlt isT doT f vs (ALit l c)              = liftM (ALit l) (redTailCall isT doT f vs c)
redTailAlt isT doT f vs (AWild c)               = liftM AWild (redTailCall isT doT f vs c)

updateParams [] []                      = return CCont
updateParams ((x,t):te) (e:es)
  | e == EVar x                         = updateParams te es
  | x `elem` evars es                   = do y <- newName tempSym
                                             c <- updateParams te (subst [(x,EVar y)] es)
                                             return (CBind False [(y,Val t (EVar x))] (CUpd x e c))
  | otherwise                           = do c <- updateParams te es
                                             return (CUpd x e c)


{-

                                     f (xs) {}
				       MUTLIST x = MUTLISTINIT()
  f (xs) {                             while (1) {
     ...                                  ...
     return NIL                           return MUTLISTEXTRACT(x)
     ...                                  ...
     return CONS(e, f(es))                MUTLISTEXTEND(x,e); xs = es; continue
     ...                                  ...
     return f(es)                         xs = es; continue  
     ...                                  ...
     return RAISE(n)                      return RAISE(n)
     ...                                  ...
     return e                             return MUTLISTAPPEXTRACT(x,e)
     ...                                  ...
  }                                  } 


-}

isTailRecModCONS isT f (CRet (ECast _ (ENew (Prim CONS _) _ bs)))
  | Just (Val _ e) <- lookup selTl bs   	= isT f e
isTailRecModCONS isT f (CBind False bs1 (CRet (ECast _ (ENew (Prim CONS _) _ bs))))
  | Just (Val _ (EVar y)) <- lookup selTl bs,
    Just (Val _ e) <- lookup y bs1              = isT f e
isTailRecModCONS isT f (CRun _ c)           	= isTailRecModCONS isT f c
isTailRecModCONS isT f (CBind _ _ c)        	= isTailRecModCONS isT f c
isTailRecModCONS isT f (CUpd _ _ c)         	= isTailRecModCONS isT f c
isTailRecModCONS isT f (CUpdS _ _ _ c)      	= isTailRecModCONS isT f c
isTailRecModCONS isT f (CUpdA _ _ _ c)      	= isTailRecModCONS isT f c
isTailRecModCONS isT f (CSwitch _ alts)     	= or [isTailRecModCONSAlt isT f a | a <- alts]
isTailRecModCONS isT f (CSeq c1 c2)         	= isTailRecModCONS isT f c1 || isTailRecModCONS isT f c2
isTailRecModCONS isT f (CWhile _ c1 c2)     	= isTailRecModCONS isT f c2
isTailRecModCONS isT f (CBreak)             	= True
isTailRecModCONS isT f (CCont)              	= True
isTailRecModCONS isT f (CRaise _)           	= True
isTailRecModCONS isT f _                    	= False

isTailRecModCONSAlt isT f (ACon _ _ _ c)    	= isTailRecModCONS isT f c
isTailRecModCONSAlt isT f (ALit _ c)        	= isTailRecModCONS isT f c
isTailRecModCONSAlt isT f (AWild c)         	= isTailRecModCONS isT f c

redTailCallModCONS isT doT f x t vs (CRet (ECast _ (ENew (Prim CONS _) _ bs)))
  | Just (Val _ e) <- lookup selTl bs, isT f e		= do c <- doT vs e
                                                     	     return (CRun (ECall (prim MUTLISTEXTEND) [t] [EVar x, e']) c)
  where Val _ e'					= lookup' bs selHd
redTailCallModCONS isT doT f x t vs (CBind False bs1 (CRet (ECast _ (ENew (Prim CONS _) _ bs))))
  | Just (Val _ (EVar y)) <- lookup selTl bs,
    Just (Val _ e) <- lookup y bs1, isT f e     = do c <- doT vs e
                                                     return (CRun (ECall (prim MUTLISTEXTEND) [t] [EVar x, e']) c)
  where Val _ e'					= lookup' bs selHd                                                
redTailCallModCONS isT doT f x t vs (CRet e@(ECast _ (ENew (Prim NIL _) ts [])))
                                                	= return (CRet (ECall (prim MUTLISTEXTRACT) [t] [(EVar x)]))
redTailCallModCONS isT doT f x t vs (CRet e)
  | isT f e						= doT vs e
redTailCallModCONS isT doT f x t vs (CRet e)            = return (CRet (ECall (prim MUTLISTAPPEXTRACT) [t] [EVar x, e]))
redTailCallModCONS isT doT f x t vs (CBind r bs c)      = liftM (CBind r bs) (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONS isT doT f x t vs (CRun e c)          = liftM (CRun e) (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONS isT doT f x t vs (CUpd y e c)        = liftM (CUpd y e) (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONS isT doT f x t vs (CUpdS e y v c)     = liftM (CUpdS e y v) (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONS isT doT f x t vs (CUpdA e i e' c)    = liftM (CUpdA e i e') (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONS isT doT f x t vs (CSwitch e alts)    = liftM (CSwitch e) (mapM (redTailCallModCONSAlt isT doT f x t vs) alts)
redTailCallModCONS isT doT f x t vs (CSeq c c')         = liftM2 CSeq (redTailCallModCONS isT doT f x t vs c) (redTailCallModCONS isT doT f x t vs c')
redTailCallModCONS isT doT f x t vs (CWhile e c c')     = liftM (CWhile e c) (redTailCallModCONS isT doT f x t vs c')
redTailCallModCONS isT doT f x t vs c                   = return c

redTailCallModCONSAlt isT doT f x t vs (ACon y us te c) = liftM (ACon y us te) (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONSAlt isT doT f x t vs (ALit l c)       = liftM (ALit l) (redTailCallModCONS isT doT f x t vs c)
redTailCallModCONSAlt isT doT f x t vs (AWild c)        = liftM AWild (redTailCallModCONS isT doT f x t vs c)


single x e                              = False -- length (filter (==x) (evars e)) == 1

newRef (ENew (Prim Ref _) _ _)          = True
newRef _                                = False

-- Convert a command
redCmd env c
  | not (null es)                       = liftM CRaise (redExp env (head es))
  where es                              = raises (subexps c)
redCmd env (CRet e)                     = do e <- redExp env e
                                             redRet env e
redCmd env e0@(CBind False [(x,Val _ e)] (CRet e'))
  | single x e' && not (newRef e)       = redCmd env (CRet (subst [(x,e)] e'))
redCmd env (CBind r bs c)               = do bs <- mapM (redBind env) bs
 					     c <- redCmd env c
                                             redGen r bs c
redCmd env (CRun e c)                   = liftM2 CRun (redExp env e) (redCmd env c)
redCmd env (CUpd x e c)                 = liftM2 (CUpd x) (redExp env e) (redCmd env c)
redCmd env (CUpdS e x v c)              = do f <- redAssign env (arrayDepth t) (ESel e x) v
                                             liftM f (redCmd env c)
  where ValT t                          = findSel env x
redCmd env (CUpdA e i e' c)             = do e <- redExp env e
                                             liftM2 (CUpdA e i) (redExp env e') (redCmd env c)
redCmd env (CSwitch e alts)             = liftM2 CSwitch (redExp env e) (mapM (redAlt env) alts)
redCmd env (CSeq c c')                  = do c <- redCmd env c
					     c' <- redCmd env c'
					     redSeq c c'
redCmd env (CBreak)                     = return CBreak
redCmd env (CRaise e)                   = liftM CRaise (redExp env e)
redCmd env (CWhile e c c')              = liftM3 CWhile (redExp env e) (redCmd env c) (redCmd env c')
redCmd env (CCont)                      = return CCont


redGen False [(x,Val _ e)] c
  | isDummy x, 
    Just cf <- redGenExp e []		= return (cf c)
redGen False [(x,Val t (EEnter (EClos vs _ te c') (Prim Code _) ts es))] c
  | not (refThis c') 			= do y <- newName functionSym
					     return (cBind [(y, Fun vs t te c')] (cBind [(x,Val t (ECall y ts es))] c))
redGen r bs c				= return (CBind r bs c)

redGenExp (EEnter e (Prim Code _) ts es) ctxt
  | all isEVal es			= redGenExp e ((ts,es):ctxt)
redGenExp (ECast _ e) ctxt		= redGenExp e ctxt
redGenExp (EClos vs _ te c) ((ts,es):ctxt)
					= redGenCmd (subst (vs `zip` ts) (subst (dom te `zip` es) c)) ctxt
redGenExp e ctxt			= Nothing

redGenCmd (CRet e) []
  | isEVal e				= Just id
redGenCmd (CRet e) ctxt			= redGenExp e ctxt
redGenCmd (CRun e c) ctxt 		= liftM (CRun e .) (redGenCmd c ctxt)
redGenCmd (CBind r bs c) ctxt		= liftM (CBind r bs .) (redGenCmd c ctxt)
redGenCmd (CUpd x e c) ctxt		= liftM (CUpd x e .) (redGenCmd c ctxt)
redGenCmd (CUpdS e x v c) ctxt		= liftM (CUpdS e x v .) (redGenCmd c ctxt)
redGenCmd (CUpdA e i e' c) ctxt		= liftM (CUpdA e i e' .) (redGenCmd c ctxt)
redGenCmd (CWhile e c c') ctxt		= liftM (CWhile e c .) (redGenCmd c' ctxt)
redGenCmd (CSwitch e alts) ctxt		= do alts' <- mapM (redGenAlt ctxt) alts
					     return (CSeq (CSwitch e alts'))
redGenCmd c ctxt			= Nothing

redGenAlt ctxt (ACon k vs te c) 	= do cf <- redGenCmd c ctxt
					     return (ACon k vs te (cf CBreak))
redGenAlt ctxt (ALit l c) 		= do cf <- redGenCmd c ctxt
					     return (ALit l (cf CBreak))
redGenAlt ctxt (AWild c) 		= do cf <- redGenCmd c ctxt
					     return (AWild (cf CBreak))

nbreaks (CBreak)			= 1
nbreaks (CBind _ _ c) 			= nbreaks c
nbreaks (CRun _ c) 			= nbreaks c
nbreaks (CUpd _ _ c) 			= nbreaks c
nbreaks (CUpdS _ _ _ c)			= nbreaks c
nbreaks (CUpdA _ _ _ c) 		= nbreaks c
nbreaks (CWhile _ _ c) 			= nbreaks c
nbreaks (CSeq _ c) 			= nbreaks c
nbreaks (CSwitch _ alts)		= sum (map nbreaks' alts)
nbreaks _				= 0

nbreaks' (ACon _ _ _ c) 		= nbreaks c
nbreaks' (ALit _ c) 			= nbreaks c
nbreaks' (AWild c) 			= nbreaks c


simpleCmd (CRet e)			= isEVal e
simpleCmd (CRaise e)			= isEVal e
simpleCmd (CBreak)			= True
simpleCmd (CCont)			= True
simpleCmd _				= False


redSeq c c'
  | simpleCmd c'			= redBreak c c'
  | nbreaks c <= 1			= redBreak c c'
  | otherwise				= return (CSeq c c')

redBreak (CBreak) ctail 		= return ctail
redBreak (CRet e) ctail			= return (CRet e)
redBreak (CBind r bs c) ctail 		= liftM (CBind r bs) (redBreak c ctail)
redBreak (CRun e c) ctail 		= liftM (CRun e) (redBreak c ctail)
redBreak (CUpd x e c) ctail 		= liftM (CUpd x e) (redBreak c ctail)
redBreak (CUpdS e x v c) ctail 		= liftM (CUpdS e x v) (redBreak c ctail)
redBreak (CUpdA e i e' c) ctail 	= liftM (CUpdA e i e') (redBreak c ctail)
redBreak (CWhile e c c') ctail 		= liftM (CWhile e c) (redBreak c' ctail)
redBreak (CSwitch e alts) ctail 	= liftM (CSwitch e) (mapM (redBreakAlt ctail) alts)
redBreak (CSeq c c') ctail 		= do c' <- redBreak c' ctail
					     redSeq c c'
redBreak (CRaise e) ctail 		= return (CRaise e)
redBreak (CCont) ctail 			= return (CCont)


redBreakAlt ctail (ACon k vs te c)	= liftM (ACon k vs te) (redBreak c ctail)
redBreakAlt ctail (ALit l' c) 		= liftM (ALit l') (redBreak c ctail)
redBreakAlt ctail (AWild c) 		= liftM (AWild) (redBreak c ctail)
	

redRet env (EEnter e f ts es)           = do c <- redRet env e
                                             return (cMap (mkEnt env f) c)
  where mkEnt env (Prim Code _) (EClos vs t te c)
	  | all isEVal es, 
	    not (refThis c) 		= subst (vs `zip` ts) (subst (dom te `zip` es) c)
	mkEnt env f (ENew n _ bs)
	  | all isEVal es,
	    not (refThis c)    		= subst (vs `zip` ts) (subst (dom te `zip` es) c)
	  where Fun vs t te c           = lookup' bs f
	mkEnt env f e                   = CRet (EEnter e f ts es)
redRet env e                            = return (CRet e)




-- Convert a switch alternative
redAlt env (ACon x vs te c)             = liftM (ACon x vs te) (redCmd env c)
redAlt env (ALit l c)                   = liftM (ALit l) (redCmd env c)
redAlt env (AWild c)                    = liftM AWild (redCmd env c)




-- Convert an expression
redExp env (EEnter e f ts es)           = do e <- redExp env e
                                             es <- mapM (redExp env) es
                                             redEnter env e f ts es
redExp env e@(ECall (Prim IndexArray _) _ _)
                                        = redIndexArray env e 0
redExp env (ECall p@(Prim SizeArray _) [t] [e])
                                        = liftM (ECall p [t] . (:[])) (redExp' env e)
redExp env (ECall x ts es)              = do es <- mapM (redExp env) es
                                             return (ECall x ts es)
redExp env (ENew n ts bs)               = do bs <- mapM (redBind env) bs
                                             redNew env n ts bs
redExp env (EClos vs t te c)		= do c <- redCmd env c
                                             redClos env vs t te c
redExp env (EVar x)                     = return (EVar x)
redExp env (EThis)                      = return (EThis)
redExp env (ELit l)                     = return (ELit l)
redExp env a@(ESel e l)
  | stateVar (annot l)                  = redIndexArray env a 0
  | otherwise                           = liftM (flip ESel l) (redExp env e)
redExp env (ECast t e)                  = liftM (ECast t) (redExp env e)


redEnter env (ENew n _ bs) f ts es
  | CRet e <- c, not (refThis e)	= return (subst (vs `zip` ts) (subst (dom te `zip` es) e))
  where Fun vs t te c                   = lookup' bs f
redEnter env (EClos vs _ te c) (Prim Code _) ts es
  | CRet e <- c, 
    all isEVal es,
    not (refThis e)			= return (subst (vs `zip` ts) (subst (dom te `zip` es) e))
redEnter env e f ts es                  = return (EEnter e f ts es)


redClos env vs _ te (CRet (EEnter e (Prim Code _) ts es))
  | ts == map tVar vs && es == map EVar (dom te) && not (refThis e)
					= return e
redClos env vs t te c			= return (EClos vs t te c)

redNew env _ _ [(Prim Code _, Fun vs _ te (CRet (EEnter e (Prim Code _) ts es)))]
  | ts == map tVar vs && es == map EVar (dom te) && not (refThis e)
                                        = return e
redNew env n ts bs                      = return (ENew n ts bs)


-- Convert an expression in a safe context (no cloning needed)
redExp' env (ECast t e)                 = liftM (ECast t) (redExp' env e)
redExp' env (ESel e l)                  = liftM (flip ESel l) (redExp env e)
redExp' env e                           = redExp env e


-- Convert an array indexing expression
redIndexArray env (ECall (Prim IndexArray _) [t] [a,i]) n
                                        = do a <- redIndexArray env a (n+1)
                                             i <- redExp env i
                                             return (indexArray t a i)
redIndexArray env (ESel e l) n
  | stateVar (annot l)                  = do e <- redExp env e
                                             return (clone (arrayDepth t - n) t (ESel e l))
  where ValT t                          = findSel env l
redIndexArray env a n                   = redExp env a


indexArray t a i                        = ECall (prim IndexArray) [t] [a,i]


intExp i                                = ELit (lInt i)

arrayDepth (TCon (Prim Array _) [t])    = 1 + arrayDepth t
arrayDepth _                            = 0

clone 0 t e                             = e
clone 1 t e@(ECall (Prim p _) _ _)
  | p `elem` [ListArray,UniArray]       = e
clone _ t e@(ECall (Prim p _) _ _)
  | p `elem` [EmptyArray,SizeArray]     = e
clone n t (ECall (Prim CloneArray _) _ [e,ELit (LInt _ n')])
  | toInteger n == n'                   = e
clone n t e                             = (ECall (prim CloneArray) [stripArray n t] [e, intExp n])

stripArray 0 t                          = t
stripArray n (TCon (Prim Array _) [t])  = stripArray (n-1) t

{-
    a|x|y|z := e
    a|x|y   := a|x|y \\ (z,e)
    a|x     := a|x \\ (y, a|x|y \\ (z,e))
    a       := a \\ (x, a|x \\ (y, a|x|y \\ (z,e)))
-}

redAssign env n e0 (ECall (Prim UpdateArray _) [t] [a,i,v])
  | e0 == a                         = redAssign env (n-1) (indexArray t a i) v
  | otherwise                       = do f1 <- redAssign env n e0 a
                                         f2 <- redAssign env (n-1) (indexArray t e0 i) v
                                         return (f1 . f2)
redAssign env n e0 (ECall (Prim ListArray _) [t] [e])
  | Just es <- constElems e, 
    let m = length es               = do f <- redAssign env n e0 (ECall (prim EmptyArray) [t] [ELit (lInt (toInteger m))])
                                         fs <- mapM mkAssign ([0..] `zip` es)
                                         return (foldl (.) f fs)
  where constElems (ENew (Prim CONS _) _ bs)
                                    = do es <- constElems eb
                                         return (ea:es)
          where Val _ ea            = lookup' bs selHd
                Val _ eb            = lookup' bs selTl
        constElems (ENew (Prim NIL _) _ bs)
                                    = Just []
        constElems _                = Nothing
        mkAssign (i,e)              = redAssign env (n-1) (indexArray t e0 (intExp i)) e
redAssign env n e0 (ECall (Prim UniArray _) [t] [m,e])
                                    = do x <- newName tempSym
                                         s <- newName tempSym
                                         i <- newName paramSym
                                         let f0 = cBind [(x,Val t e),(s,Val tInt m),(i,Val tInt (intExp 0))]
                                         f <- redAssign env n e0 (ECall (prim EmptyArray) [t] [EVar s])
                                         let f1 c = CWhile (ECall (prim IntLT) [] [EVar i, EVar s]) c
                                         f2 <- redAssign env (n-1) (indexArray t e0 (EVar i)) (EVar x)
                                         let c = CUpd i (ECall (prim IntPlus) [] [EVar i, intExp 1]) CCont
                                         return (f0 . f . f1 (f2 c))
redAssign env n (ESel e x) v        = do v' <- redExp env v
                                         return (CUpdS e x (clone n t v'))
  where ValT t                      = findSel env x
redAssign env n (ECall (Prim IndexArray _) [t] [e,i]) v
                                    = do v' <- redExp env v
                                         return (CUpdA e i (clone n t v'))

