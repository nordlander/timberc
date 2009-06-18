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

import Monad
import Common
import Syntax
import Match
import Maybe
import Fixity
import PP

desugar2 :: Module -> M s Module
desugar2 m = dsModule m


-- Modules ------------------------------------------------------------------

dsModule :: Module -> M s Module
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
dsDecls (DBind bs : ds)         = do bs <- dsBinds bs
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

dsBinds []                      = return []
dsBinds (BSig vs qt : bs)       = do bs <- dsBinds bs
                                     return (BSig vs (dsQualWildType qt) : bs)
dsBinds bs                      = f [] bs
  where f eqns (BEqn lh rh :bs) = f ((lh,rh) : eqns) bs
        f eqns bs               = do eqns <- dsEqns (reverse eqns)
                                     bs <- dsBinds bs
                                     return (map (uncurry BEqn) eqns ++ bs)


-- Equations -----------------------------------------------------------------

dsEqns []                       = return []
dsEqns ((LPat p,RExp (EVar v)):eqns)
                                = do p' <- dsPat p      -- Checks validity
                                     sels <- mapM (sel (EVar v)) vs
                                     dsEqns (sels ++ eqns)
  where vs                      = pvars p
        sel e0 v                = do es <- mapM (newEVarPos paramSym) vs
                                     return (LFun v [], RExp (selectFrom e0 (vs `zip` es) p (EVar v)))
dsEqns ((LPat p,rh):eqns)       = do v <- newNamePos patSym p
                                     eqns <- dsEqns ((LPat p, RExp (EVar v)) : eqns)
                                     e <- dsExp (rh2exp rh)
                                     return ((LFun v [],RExp e) : eqns)
dsEqns ((LFun v ps,rh):eqns)    = dsFunBind v [(ps,rh)] eqns


dsFunBind v alts ((LFun v' ps,rh) : eqns)
  | v' == v                     = dsFunBind v ((ps,rh) : alts) eqns
dsFunBind v [(ps,RExp e)] eqns
  | not (null ps)               = do e' <- dsExp (ELam ps e)
                                     eqns <- dsEqns eqns
                                     return ((LFun v [], RExp e') : eqns)
dsFunBind v alts eqns
  | length arities /= 1         = errorIds "Different arities for function" [v]
  | otherwise                   = do ws <- newNamesPos paramSym (fst (head alts))
                                     alts <- mapM dsA (reverse alts)
                                     e <- pmc' ws alts
                                     eqns <- dsEqns eqns 
                                     return ((LFun v [], RExp (eLam (map EVar ws) e)) : eqns)
  where arities                 = nub (map (length . fst) alts)
        dsA (ps,rh)             = liftM2 (,) (mapM dsPat ps) (dsRh rh)


-- Helper functions --------------------------------------------------

checkQual t ps
  | not (null ambig)            = errorIds "Ambiguous type scheme, orphans are" ambig
  | otherwise                   = TQual t ps
  where ambig                   = tvs_ps \\ (vclose tvss tvs)
        tvs_ps                  = nub (concat tvss `intersect` bvs)
        tvs                     = tyvars t
        tvss                    = map tyvars ps
        bvs                     = bvars ps


zipSigs (v:vs) (ESig _ t : ps)  = ESig (EVar v) t : zipSigs vs ps
zipSigs (v:vs) (p : ps)         = EVar v : zipSigs vs ps
zipSigs _      _                = []

rh2exp (RExp e)                 = e
rh2exp (RWhere rh bs)           = ELet bs (rh2exp rh)
rh2exp rh                       = ECase (ECon (prim UNITTERM)) [Alt (ECon (prim UNITTERM)) rh]

selectFrom e0 s p e             = ECase e0 [Alt (subst s p) (RExp (subst s e))]


-- Expressions --------------------------------------------------------


dsExp (EAp e e')                = liftM2 EAp (dsExp e) (dsExp e')
dsExp (ESig e qt)               = do x <- newNamePos tempSym e
                                     dsExp (ELet [BSig [x] qt, BEqn (LFun x []) (RExp e)] (EVar x))
dsExp (ELam ps e)            
  | all isESigVar ps            = liftM2 ELam (mapM dsPat ps) (dsExp e)
  | otherwise                   = do ps <- mapM dsPat ps
                                     e <- dsExp e
                                     ws <- newNamesPos paramSym ps
                                     e' <- pmc' ws [(ps,RExp e)]
                                     return (ELam (zipSigs ws ps) e')
dsExp (ELet [BEqn (LPat p) rh] e)
  | nonRecursive                = dsExp (ECase (rh2exp rh) [Alt p (RExp e)])
  where nonRecursive            = not (any (`elem` evars rh) (pvars p))
dsExp (ELet bs e)               = liftM2 ELet (dsBinds bs) (dsExp e)
dsExp (EIf e e1 e2)             = dsExp (ECase e [Alt true  (RExp e1), Alt false (RExp e2)])
dsExp (ESectR e op)             = dsExp (EAp (op2exp op) e)
dsExp (ESectL op e)             = do x <- newEVarPos paramSym op
                                     dsExp (ELam [x] (EAp (EAp (op2exp op) x) e))
dsExp (ECase e alts)            = do e <- dsExp e
                                     alts <- mapM dsA alts
                                     pmc e alts
  where dsA (Alt p rh)          = liftM2 Alt (dsPat p) (dsRh rh)
dsExp (ESelect e s)             = liftM (flip ESelect s) (dsExp e)
dsExp (ESel s)                  = do x <- newNamePos paramSym s
                                     return (ELam [EVar x] (ESelect (EVar x) s))
dsExp (EWild)                   = errorTree "Non-pattern use of wildcard variable" EWild
dsExp (EVar v)                  = return (EVar v)
dsExp (ECon c)                  = return (ECon c)
dsExp (ELit l)                  = return (ELit l)
dsExp (ERec m fs)               = liftM (ERec m) (mapM dsF fs)
  where dsF (Field s e)         = liftM (Field s) (dsExp e)
dsExp (EDo v t ss)              = liftM (EDo v (fmap dsWildType t)) (dsStmts False ss)
dsExp (ETempl v t ss)           = liftM (ETempl v (fmap dsWildType t)) (dsStmts True ss)
dsExp (EAct v ss)               = liftM (EAct v) (dsStmts False ss)
dsExp (EReq v ss)               = liftM (EReq v) (dsStmts False ss)
dsExp (EAfter e e')             = dsExp (EAp (EAp (EVar (prim After)) e) e')
dsExp (EBefore e e')            = dsExp (EAp (EAp (EVar (prim Before)) e) e')
dsExp (ETup es)                 = dsExp (foldl EAp (ECon (tuple (length es))) es)
dsExp (EList es)                = dsExp (foldr cons nil es)
dsExp (EComp e qs)              = do e <- comp2exp e qs nil
                                     dsExp e


-- List comprehensions --------------------------------------------------

comp2exp e [] r                 = return (cons e r)
comp2exp e (QExp e' : qs) r     = do e <- comp2exp e qs r
                                     return (EIf e' e r)
comp2exp e (QLet bs : qs) r     = do e <- comp2exp e qs r
                                     return (ELet bs e)
comp2exp e (QGen p e' : qs) r   = do f <- newNamePos functionSym p
                                     x <- newEVarPos paramSym p
                                     e <- comp2exp e qs (EAp (EVar f) x)
                                     return (ELet (binds f x e) (EAp (EVar f) e'))
  where binds f x e             = [BEqn (LFun f [nil]) (RExp r),
                                   BEqn (LFun f [cons p x]) (RExp e)] ++ dflt f x
        dflt f x | isEVar p     = []
                 | otherwise    = [BEqn (LFun f [cons EWild x]) (RExp (EAp (EVar f) x))]


-- Statements ------------------------------------------------------------

dsStmts cl []                   = return []
dsStmts cl (SBind [BEqn (LPat p) rh] : ss)
  | not cl && nonRecursive      = do x <- newName tempSym
                                     dsStmts cl [SExp (ECase (rh2exp rh) [Alt p (RExp (EDo (Just x) Nothing ss))])]
  where nonRecursive            = not (any (`elem` evars rh) (pvars p))
dsStmts cl (SBind bs : ss)      = do bs <- dsBinds bs
                                     liftM (SBind bs :) (dsStmts cl ss)
dsStmts cl [SRet e]             = do e <- dsExp e
                                     return [SRet e]
dsStmts cl [SExp e]             = do e <- dsExp e
                                     return [SExp e]
dsStmts cl (SGen p e : ss)
  | isESigVar p                 = do p <- dsInnerPat p
                                     e <- dsExp e
                                     ss <- dsStmts cl ss
                                     return (SGen p e : ss)
  | otherwise                   = do v' <- newEVarPos tempSym p
                                     dsStmts cl (SGen v' e : SBind [BEqn (LPat p) (RExp v')] : ss)
dsStmts cl (s@(SAss p e) : ss)
  | null vs                     = errorTree "Bad assignment" s
  | not cl && p0 /= p           = errorTree "Illegal signature in assignment" s
  | isESigVar p                 = do p <- dsPat p
                                     e <- dsExp e
                                     ss <- dsStmts cl ss
                                     return (SAss p e : ss)
  | otherwise                   = do v0 <- newNamePos tempSym p
                                     assigns <- mapM (assign (EVar v0)) ps
                                     dsStmts cl (SBind [BEqn (LFun v0 []) (RExp e)] : assigns ++ ss)
  where assign e0 p             = do vs' <- newNamesPos paramSym vs
                                     return (SAss p (selectFrom e0 (vs `zip` map EVar vs') p0 (unsig p)))
        p0                      = unsig p
        ps                      = sigvars p
        vs                      = pvars ps
dsStmts cl ss                   = internalError ("dsStmts; did not expect") ss

unsig (ESig p _)                = unsig p
unsig (ETup ps)                 = ETup (map unsig ps)
unsig (EList ps)                = EList (map unsig ps)
unsig (ERec m fs)               = ERec m [ Field l (unsig p) | Field l p <- fs ]
unsig p                         = p

sigvars (ETup ps)               = concatMap sigvars ps
sigvars (EList ps)              = concatMap sigvars ps
sigvars (ERec m fs)             = concat [ sigvars p | Field l p <- fs ]
sigvars p
  | isESigVar p                 = [p]
  | otherwise                   = []


-- Alternatives -----------------------------------------------------------------------

dsRh (RExp e)                   = liftM RExp (dsExp e)
dsRh (RGrd gs)                  = liftM RGrd (mapM dsGrd gs)
dsRh (RWhere rh [BEqn (LPat p) rh'])
  | nonRecursive                = liftM RExp (dsExp (ECase (rh2exp rh') [Alt p (RExp (rh2exp rh))]))
  where nonRecursive            = not (any (`elem` evars rh') (pvars p))  
dsRh (RWhere rh bs)             = liftM2 RWhere (dsRh rh) (dsBinds bs)

dsGrd (GExp qs e)               = do qs <- mapM dsEQual qs
                                     e <- dsExp e
                                     return (GExp qs e)

dsEQual (QExp e)                = do e <- dsExp e
                                     return (QGen true e)
dsEQual (QGen p e)              = do p <- dsPat p
                                     e <- dsExp e
                                     return (QGen p e)
dsEQual (QLet [BEqn (LPat p) rh])
  | nonRecursive                = do p <- dsPat p
                                     e <- dsExp (rh2exp rh)
                                     return (QGen p e)
  where nonRecursive            = not (any (`elem` evars rh) (pvars p))  
dsEQual (QLet bs)               = liftM QLet (dsBinds bs)


-- Patterns ----------------------------------------------------------------------------

dsPat (ESig p qt)
  | isEVar p                    = do p <- dsPat p
                                     return (ESig p (dsQualWildType qt))
dsPat (EVar v)                  = return (EVar v)
dsPat (EWild)                   = do v <- newName dummySym
                                     return (EVar v)
dsPat (ENeg (ELit (LInt p i)))  = return (ELit (LInt p (-i)))
dsPat (ENeg (ELit (LRat p r)))  = return (ELit (LRat p (-r)))
dsPat (ELit l)                  = return (ELit l)
dsPat (ETup ps)                 = dsPat (foldl EAp (ECon (tuple (length ps))) ps)
dsPat (EList ps)                = dsPat (foldr cons nil ps)
dsPat (ERec m fs)               = liftM (ERec m) (mapM dsField fs)
dsPat p                         = dsConPat p

dsField (Field l p)             = liftM (Field l) (dsPat p)


dsConPat (EAp p p')             = liftM2 EAp (dsConPat p) (dsInnerPat p')
dsConPat (ECon c)               = return (ECon c)
dsConPat p                      = errorTree "Illegal pattern" p

dsInnerPat (ESig p t)
  | isEVar p                    = do p <- dsPat p
                                     return (ESig p (dsWildType t))
dsInnerPat p                    = dsPat p


-- Literals ------------------------------------------------------------------

--dsLit (LStr (Just (l,c)) s)     = return (foldr cons nil (map mkLit (zipWith f s [c..])))
--  where f s n                   = (s,Just (l,n))
--        mkLit (c,p)             = (ELit (LChr p c))
--dsLit l                         = return (ELit l)


