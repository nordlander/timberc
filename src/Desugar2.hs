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

module Desugar2(desugar2) where

import Control.Monad
import Common
import Syntax
import Match
import Data.Maybe
import Fixity
import PP

desugar2 :: Module -> M s Module
desugar2 m = localStore (dsModule m)


-- Modules ------------------------------------------------------------------

--dsModule :: Module -> M s Module
dsModule (Module c is ds ps)    = do ds <- dsDecls ds
                                     return (Module c is ds ps)


-- Top level declarations ---------------------------------------------------



dsDecls []                      = return []
dsDecls (d@(DKSig _ _) : ds)    = liftM (d :) (dsDecls ds)
dsDecls (DData c vs bs cs : ds) = liftM (DData c vs (map dsQualBaseType bs) (map dsConstr cs) :) (dsDecls ds)
dsDecls (DRec i c vs bs ss:ds)  = liftM (DRec i c vs (map dsQualBaseType bs) (map dsSig ss) :) (dsDecls ds)
dsDecls (DType c vs t : ds)     = liftM (DType c vs (dsType t) :) (dsDecls ds)
dsDecls (DPSig v t : ds)        = liftM (DPSig v (dsQualPred t) :) (dsDecls ds)
dsDecls (DInstance vs : ds)     = liftM (DInstance vs :) (dsDecls ds)
dsDecls (DTClass vs : ds)       = dsDecls ds
dsDecls (DDefault ts : ds)      = liftM (DDefault (map dsDefault ts) :) (dsDecls ds) 
dsDecls (DExtern es : ds)      = liftM (DExtern (map dsExtern es) :) (dsDecls ds) 
dsDecls (DBind bs : ds)         = do bs <- dsBinds Nothing bs
                                     liftM (DBind bs :) (dsDecls ds)
        

dsConstr (Constr c ts ps)       = Constr c (map dsQualType ts) (map dsQual ps)

dsSig (Sig vs t)                = Sig vs (dsQualType t)

dsDefault (Default t a b)       = Default t a b
dsDefault (Derive v t)          = Derive v (dsQualType t)

dsExtern (Extern v t)           = Extern v (dsQualType t)


-- Types ----------------------------------------------------------------------

dsQualType (TQual t ps)         = checkQual (dsRhoType t) (map dsQual ps)
dsQualType t                    = dsRhoType t

dsRhoType (TFun ts t)           = TFun (map dsQualType ts) (dsRhoType t)
dsRhoType t                     = dsType t

dsType (TAp t t')               = TAp (dsType t) (dsType t')
dsType (TFun ts t)              = TFun (map dsType ts) (dsType t)
dsType (TList t)                = TAp (TCon (prim LIST)) (dsType t)
dsType (TTup ts)                = foldl TAp (TCon (tuple (length ts))) (map dsType ts)
dsType (TCon c)                 = TCon c
dsType (TVar v)                 = TVar v
dsType (TWild)                  = errorTree "Illegal use of type wildcard" TWild
dsType t                        = errorTree "Illegal type expression" t


-- Types with wildcards ---------------------------------------------------------------

dsQualWildType (TQual t ps)     = checkQual (dsRhoWildType t) (map dsQual ps)
dsQualWildType t                = TQual (dsRhoWildType t) []

dsRhoWildType (TFun ts t)       = TFun (map dsQualWildType ts) (dsRhoWildType t)
dsRhoWildType t                 = dsWildType t

dsWildType (TAp t t')           = TAp (dsWildType t) (dsWildType t')
dsWildType (TFun ts t)          = TFun (map dsWildType ts) (dsWildType t)
dsWildType (TList t)            = TAp (TCon (prim LIST)) (dsWildType t)
dsWildType (TTup ts)            = foldl TAp (TCon (tuple (length ts))) (map dsWildType ts)
dsWildType (TCon c)             = TCon c
dsWildType (TVar v)             = TVar v
dsWildType (TWild)              = TWild
dsWildType t                    = errorTree "Illegal type expression" t


-- Base types -------------------------------------------------------------

dsQualBaseType (TQual t ps)     = checkQual (dsBaseType t) (map dsKindQual ps)
dsQualBaseType t                = TQual (dsBaseType t) []

dsKindQual (PKind v k)          = PKind v k
dsKindQual (PType (TVar v))     = PKind v KWild
dsKindQual q                    = errorTree "Illegal base type qualifier" q

dsBaseType (TAp t t')           = TAp (dsBaseType t) (dsType t')
dsBaseType (TList t)            = TAp (TCon (prim LIST)) (dsType t)
dsBaseType (TCon c)             = TCon c
dsBaseType t                    = errorTree "Illegal base type expression" t


-- Predicates ------------------------------------------------------------

dsQual (PKind v k)              = PKind v k
dsQual (PType (TVar v))         = PKind v KWild
dsQual (PType t)                = PType (dsQualPred t)

dsQualPred (TQual t ps)         = checkQual (dsSubOrClassPred t) (map dsQual ps)
dsQualPred t                    = TQual (dsSubOrClassPred t) []

dsSubOrClassPred (TSub t t')    = TSub (dsType t) (dsType t')
dsSubOrClassPred t              = dsClassPred t

dsClassPred (TAp t t')          = TAp (dsClassPred t) (dsType t')
dsClassPred (TCon c)            = TCon c
dsClassPred p                   = errorTree "Illegal type qualifier" p


-- Bindings ---------------------------------------------------------------

{-

The parameter vs :: Maybe [Name] given as first argument to most functions 
has the following meaning:
 - its value is Just xs when used to desugar a command c  and its constituent parts; here
   xs is the variable bound in an outer scope in the present command; when we encounter
   a pre-expression (EGen e or ENew e) we check that the free variables in e do not
   intersects with xs; this prohibits lifting of the pre-expression to a command in
   the same sequence as c.
 - its value is Nothing otherwise.
   
-}

dsBinds _ []                    = return []
dsBinds vs (BSig vs' qt : bs)   = do bs <- dsBinds vs bs
                                     return (BSig vs' (dsQualWildType qt) : bs)
dsBinds vs bs                   = f [] bs
  where f eqns (BEqn lh rh :bs) = f ((lh,rh) : eqns) bs
        f eqns bs               = do eqns <- dsEqns vs (reverse eqns)
                                     bs <- dsBinds vs bs
                                     return (map (uncurry BEqn) eqns ++ bs)


-- Equations -----------------------------------------------------------------

dsEqns vs []                    = return []
dsEqns vs ((LPat (PVar v), r):eqns)
				= dsEqns vs ((LFun v [], r):eqns)
dsEqns vs ((LPat (PRec _ ps),RExp (EVar v)):eqns)
				= dsEqns vs ([ (LPat p, RExp (ESelect (EVar v) l)) | Field l p <- ps ] ++ eqns)
dsEqns vs ((LPat p,RExp (EVar v)):eqns)
                                = do p' <- dsPat p      -- Checks validity
                                     sels <- mapM (sel (EVar v)) vs'
                                     dsEqns vs (sels ++ eqns)
  where vs'                     = pvars p
        sel e0 v'               = do vs'' <- newNamesPos dummySym vs'
                                     v'' <- newNamePos paramSym v'
                                     return (LFun v' [], RExp (selectFrom e0 (zip (v':vs') (v'':vs'')) p (EVar v')))
dsEqns vs ((LPat p,rh):eqns)    = do v <- newNamePos patSym p
                                     eqns <- dsEqns vs ((LPat p, RExp (EVar v)) : eqns)
                                     e <- dsExp vs (rh2exp rh)
                                     return ((LFun v [],RExp e) : eqns)
dsEqns vs ((LFun v ps,rh):eqns) = dsFunBind vs v [(ps,rh)] eqns


dsFunBind vs v alts ((LFun v' ps,rh) : eqns)
  | v' == v                     = dsFunBind vs v ((ps,rh) : alts) eqns
dsFunBind vs v [(ps,RExp e)] eqns
  | not (null ps)               = do e' <- dsExp vs (ELam ps e)
                                     eqns <- dsEqns vs eqns
                                     return ((LFun v [], RExp e') : eqns)
dsFunBind vs v alts eqns
  | length arities /= 1         = errorIds "Different arities for function" [v]
  | otherwise                   = do ws <- newNamesPos paramSym (fst (head alts))
                                     alts <- mapM dsA (reverse alts)
                                     e <- pmc' ws alts
                                     eqns <- dsEqns vs eqns 
                                     return ((LFun v [], RExp (eLam (map PVar ws) e)) : eqns)
  where arities                 = nub (map (length . fst) alts)
        dsA (ps,rh)             = liftM2 (,) (mapM dsPat ps) (dsRh vs rh)


-- Helper functions --------------------------------------------------

checkQual t ps
  | not (null ambig)            = errorIds "Ambiguous type scheme, orphans are" ambig
  | otherwise                   = TQual t ps
  where ambig                   = tvs_ps \\ (vclose tvss tvs)
        tvs_ps                  = nub (concat tvss `intersect` bvs)
        tvs                     = tyvars t
        tvss                    = map tyvars ps
        bvs                     = bvars ps


zipSigs (v:vs) (PSig _ t : ps)  = PSig (PVar v) t : zipSigs vs ps
zipSigs (v:vs) (p : ps)         = PVar v : zipSigs vs ps
zipSigs _      _                = []

rh2exp (RExp e)                 = e
rh2exp (RWhere rh bs)           = ELet bs (rh2exp rh)
rh2exp rh                       = ECase unit [Alt unitP rh]

selectFrom e0 s p e             = ECase e0 [Alt (subst (mapSnd PVar s) p) (RExp (subst (mapSnd EVar s) e))]

getAndClearStore                = do ss <- currentStore
                                     clearStore
                                     return (reverse ss)

joinVars (Just xs) ys           = Just (nub (xs ++ ys))
joinVars Nothing ys             = Nothing

-- Expressions --------------------------------------------------------

dsExp vs (EAp (EAp (EVar (Prim LazyAnd _)) e1) e2) 
                                = do e1 <- dsExp vs e1
                                     e2 <- dsExp Nothing e2
                                     return (EAp (EAp (EVar (prim LazyAnd)) e1) e2)
dsExp vs (EAp (EAp (EVar (Prim LazyOr _)) e1) e2) 
                                = do e1 <- dsExp vs e1
                                     e2 <- dsExp Nothing e2
                                     return (EAp (EAp (EVar (prim LazyOr)) e1) e2)
dsExp vs (EAp e e')             = liftM2 EAp (dsExp vs e) (dsExp vs e')
dsExp vs (ESig e qt)            = do x <- newNamePos tempSym e
                                     dsExp vs (ELet [BSig [x] qt, BEqn (LFun x []) (RExp e)] (EVar x))
dsExp vs (ELam ps e)            
  | all isPSigVar ps            = liftM2 ELam (mapM dsPat ps) (dsExp Nothing e)
  | otherwise                   = do ps <- mapM dsPat ps
                                     e <- dsExp Nothing e
                                     ws <- newNamesPos paramSym ps
                                     e' <- pmc' ws [(ps,RExp e)]
                                     return (ELam (zipSigs ws ps) e')
dsExp vs (ELet [BEqn (LPat p) rh] e)
  | nonRecursive                = dsExp vs (ECase (rh2exp rh) [Alt p (RExp e)])
  where nonRecursive            = not (any (`elem` evars rh) (pvars p))
dsExp vs (ELet bs e)            = do liftM2 ELet (dsBinds vs' bs) (dsExp vs' e)
  where vs' = joinVars vs (bvars bs)
dsExp vs (EIf e e1 e2)          = dsExp vs (ECase e [Alt trueP  (RExp e1), Alt falseP (RExp e2)])
dsExp vs (ESectR e op)          = dsExp vs (EAp (op2exp op) e)
dsExp vs (ESectL op e)          = do x <- newNamePos paramSym op
                                     dsExp vs (ELam [PVar x] (EAp (EAp (op2exp op) (EVar x)) e))
dsExp vs (ECase e alts)         = do e <- dsExp vs e
                                     alts <- dsAlts vs (dsExp Nothing) alts
                                     pmc e alts
dsExp vs (EMatch m)             = EMatch `fmap` dsMatch vs (dsExp vs) m
dsExp vs (ESelect e s)          = liftM (flip ESelect s) (dsExp vs e)
dsExp _ (ESel s)                = do x <- newNamePos paramSym s
                                     return (ELam [PVar x] (ESelect (EVar x) s))
dsExp _ EWild                   = errorTree "Non-pattern use of wildcard variable" EWild
dsExp _ (EVar v)                = return (EVar v)
dsExp _ (ECon c)                = return (ECon c)
dsExp _  (ELit l)               = return (ELit l)
dsExp vs (ERec m fs)            = liftM (ERec m) (mapM dsF fs)
  where dsF (Field s e)         = liftM (Field s) (dsExp vs e)
dsExp _ (EDo v t ss)            = liftM (EDo v (fmap dsWildType t)) (dsStmts False ss)
dsExp _ (ETempl v t ss)         = liftM (ETempl v (fmap dsWildType t)) (dsStmts True ss)
dsExp _ (EAct v ss)             = liftM (EAct v) (dsStmts False ss)
dsExp _ (EReq v ss)             = liftM (EReq v) (dsStmts False ss)
dsExp vs (EAfter e e')          = dsExp vs (EAp (EAp (EVar (prim After)) e) e')
dsExp vs (EBefore e e')         = dsExp vs (EAp (EAp (EVar (prim Before)) e) e')
dsExp vs (ETup es)              = dsExp vs (foldl EAp (ECon (tuple (length es))) es)
dsExp vs (EList es)             = dsExp vs (foldr cons nil es)
dsExp vs (EComp e qs)           = do e <- comp2exp e qs nil
                                     dsExp vs e
dsExp Nothing e@(ENew _)        = errorTree "Illegal context for pre-expression" e
dsExp Nothing e@(EGen _)        = errorTree "Illegal context for pre-expression" e
dsExp vs@(Just xs) (EGen e)      
  | not (null clashing)         = errorIds "Pre-expression cannot be lifted; vars in local scope" clashing
  | otherwise                   = do n <- newNamePos tempSym e
                                     e' <- dsExp vs e
                                     addToStore (SGen (PVar n) e')
                                     return (EVar n)
   where clashing               = evars e `intersect` xs 
dsExp vs@(Just xs) (ENew e)
  | not (null clashing)         = errorIds "Pre-expression cannot be lifted; vars in local scope" clashing
  | otherwise                   = do n <- newNamePos tempSym e
                                     e' <- dsExp vs e
                                     addToStore (SBind [BEqn (LFun n []) (RExp (EAp (EVar (prim New)) e'))])
                                     return (EVar n)
   where clashing               = evars e `intersect` xs 

dsAlts :: Maybe [Name] -> (a->M Stmt a) -> [Alt a] -> M Stmt [Alt a]
dsAlts vs dsE as = mapM dsAlt as
  where dsAlt (Alt p rh)          = liftM2 Alt (dsPat p) (dsRhs (joinVars vs (pvars p)) dsE rh)

-- type signature needed because of polymorphic recursion
dsMatch ::  Maybe [Name] -> (r->M Stmt r) -> Match Pat Exp [Bind] r -> M Stmt (Match Pat Exp [Bind] r)
dsMatch vs dsR = mapMMatch dsPat (dsExp vs) (dsBinds vs) dsR

-- List comprehensions --------------------------------------------------

comp2exp e [] r                 = return (cons e r)
comp2exp e (QExp e' : qs) r     = do e <- comp2exp e qs r
                                     return (EIf e' e r)
comp2exp e (QLet bs : qs) r     = do e <- comp2exp e qs r
                                     return (ELet bs e)
comp2exp e (QGen p e' : qs) r   = do f <- newNamePos functionSym p
                                     x <- newNamePos paramSym p
                                     e <- comp2exp e qs (EAp (EVar f) (EVar x))
                                     return (ELet (binds f x e) (EAp (EVar f) e'))
  where binds f x e             = [BEqn (LFun f [nilP]) (RExp r),
                                   BEqn (LFun f [consP p (PVar x)]) (RExp e)] ++ dflt f x
        dflt f x | isPVar p     = []
                 | otherwise    = [BEqn (LFun f [consP PWild (PVar x)]) (RExp (EAp (EVar f) (EVar x)))]


-- Statements ------------------------------------------------------------
dsStmts cl (Stmts ss)           = localStore (Stmts `fmap` dsSs cl ss)

dsSs cl []                      = return []
dsSs cl (SBind [BEqn (LPat p) rh] : ss)
  | not cl && nonRecursive      = do x <- newName tempSym
                                     dsSs cl [SExp (ECase (rh2exp rh) [Alt p (RExp (EDo (Just x) Nothing (Stmts ss)))])]
  where nonRecursive            = not (any (`elem` evars rh) (pvars p))
dsSs cl (SBind bs : ss)         = do bs <- dsBinds (Just []) bs
                                     ss' <- getAndClearStore
                                     liftM ((ss' ++) . (SBind bs :)) (dsSs cl ss)
dsSs cl [SRet e]                = do e <- dsExp (Just []) e
                                     ss' <- getAndClearStore
                                     return (ss' ++ [SRet e])
dsSs cl [SExp e]                = do e <- dsExp (Just []) e
                                     ss' <- getAndClearStore
                                     return (ss' ++ [SExp e])
dsSs cl (SGen p e : ss)
  | isPSigVar p                 = do p <- dsInnerPat p
                                     e <- dsExp (Just []) e
                                     ss' <- getAndClearStore
                                     ss <- dsSs cl ss
                                     return (ss' ++ SGen p e : ss)
  | otherwise                   = do v' <- newNamePos tempSym p
                                     dsSs cl (SGen (PVar v') e : SBind [BEqn (LPat p) (RExp (EVar v'))] : ss)
dsSs cl (s@(SAss p e) : ss)
  | null vs                     = errorTree "Bad assignment" s
  | not cl && p0 /= p           = errorTree "Illegal signature in assignment" s
  | isPSigVar p                 = do p <- dsPat p
                                     e <- dsExp (Just []) e
                                     ss' <- getAndClearStore
                                     ss <- dsSs cl ss
                                     return (ss' ++ SAss p e : ss)
  | otherwise                   = do v0 <- newNamePos tempSym p
                                     assigns <- mapM (assign (EVar v0)) sigvs
                                     dsSs cl (SBind [BEqn (LFun v0 []) (RExp e)] : assigns ++ ss)
  where assign e0 sigv          = do let PVar v = unsig sigv
                                     vs' <- newNamesPos dummySym vs
                                     v' <- newNamePos paramSym v
                                     return (SAss sigv (selectFrom e0 (zip (v:vs) (v':vs')) p0 (pat2exp (unsig sigv))))
        p0                      = unsig p
        sigvs                   = sigvars p
        vs                      = pvars sigvs
dsSs cl (SCase e as : ss)       = do e <- dsExp (Just []) e
                                     ss' <- getAndClearStore
                                     as <- dsAlts (Just []) (dsStmts cl) as
                                     Stmts css <- pmc e as
                                     ss <- dsSs cl ss
                                     return (ss' ++ css ++ ss)
dsSs cl (SMatch m : ss)         = do m <- dsMatch (Just []) (dsStmts cl) m
                                     liftM (SMatch m :) (dsSs cl ss)
dsSs cl ss                      = internalError ("dsSs; did not expect") (Stmts ss)

unsig (PSig p _)                = unsig p
unsig (PAp p1 p2)               = PAp (unsig p1) (unsig p2)
unsig (PTup ps)                 = PTup (map unsig ps)
unsig (PList ps)                = PList (map unsig ps)
unsig (PRec m fs)               = PRec m [ Field l (unsig p) | Field l p <- fs ]
unsig p                         = p

sigvars (PAp p1 p2)             = sigvars p1 ++ sigvars p2
sigvars (PTup ps)               = concatMap sigvars ps
sigvars (PList ps)              = concatMap sigvars ps
sigvars (PRec m fs)             = concat [ sigvars p | Field l p <- fs ]
sigvars p
  | isPSigVar p                 = [p]
  | otherwise                   = []


-- Alternatives -----------------------------------------------------------------------

dsRh vs (RExp e)           = liftM RExp (dsExp vs e)
dsRh vs (RGrd gs)          = liftM RGrd (mapM dsGrd gs)
  where
    dsGrd (GExp qs e)           = do qs <- mapM (dsEQual vs) qs
                                     e <- dsExp vs e
                                     return (GExp qs e)
dsRh vs (RWhere rh [BEqn (LPat p) rh'])
  | nonRecursive              = liftM RExp (dsExp vs (ECase (rh2exp rh') [Alt p (RExp (rh2exp rh))]))
  where nonRecursive          = not (any (`elem` evars rh') (pvars p))  
dsRh vs (RWhere rh bs)        = liftM2 RWhere (dsRh vs rh) (dsBinds vs bs)


-- Generalized version of dsRh, without where->case transformation
dsRhs :: Maybe [Name] -> (a -> M Stmt a) -> Rhs a -> M Stmt (Rhs a)
dsRhs _ dsE (RExp e)            = liftM RExp (dsE e)
dsRhs vs dsE (RGrd gs)          = liftM RGrd (mapM dsGrd gs)
  where
    dsGrd (GExp qs e)           = do qs <- mapM (dsEQual vs) qs
                                     e <- dsE e
                                     return (GExp qs e)
dsRhs vs dsE (RWhere rh bs)     = liftM2 RWhere (dsRhs vs dsE rh) (dsBinds vs bs)


dsEQual vs (QExp e)             = do e <- dsExp vs e
                                     return (QGen trueP e)
dsEQual vs (QGen p e)           = do p <- dsPat p
                                     e <- dsExp vs e
                                     return (QGen p e)
dsEQual vs (QLet [BEqn (LPat p) rh])
  | nonRecursive                = do p <- dsPat p
                                     e <- dsExp vs (rh2exp rh)
                                     return (QGen p e)
  where nonRecursive            = not (any (`elem` evars rh) (pvars p))  
dsEQual vs (QLet bs)               = liftM QLet (dsBinds vs bs)


-- Patterns ----------------------------------------------------------------------------

dsPat (PSig p qt)
  | isPVar p                    = do p <- dsPat p
                                     return (PSig p (dsQualWildType qt))
dsPat (PVar v)                  = return (PVar v)
dsPat (PWild)                   = do v <- newName dummySym
                                     return (PVar v)
dsPat (PLit l)                  = return (PLit l)
dsPat (PTup ps)                 = dsPat (foldl PAp (PCon (tuple (length ps))) ps)
dsPat (PList ps)                = dsPat (foldr consP nilP ps)
dsPat (PRec m fs)               = liftM (PRec m) (mapM dsField fs)
dsPat p                         = dsConPat p

dsField (Field l p)            = liftM (Field l) (dsPat p)


dsConPat (PAp p p')             = liftM2 PAp (dsConPat p) (dsInnerPat p')
dsConPat (PCon c)               = return (PCon c)
dsConPat p                      = errorTree "Illegal pattern" p

dsInnerPat (PSig p t)
  | isPVar p                    = do p <- dsPat p
                                     return (PSig p (dsWildType t))
dsInnerPat p                    = dsPat p


-- Literals ------------------------------------------------------------------

--dsLit (LStr (Just (l,c)) s)     = return (foldr cons nil (map mkLit (zipWith f s [c..])))
--  where f s n                   = (s,Just (l,n))
--        mkLit (c,p)             = (ELit (LChr p c))
--dsLit l                         = return (ELit l)


