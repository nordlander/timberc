module Desugar2(desugar2) where

import Monad
import Common
import Syntax
import Match
import Maybe
import Fixity

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
dsDecls (DDefault ts : ds)      = liftM (DDefault (map dsDefault ts) :) (dsDecls ds) 
dsDecls (DBind b : ds)          = do bs' <- dsBinds bs
                                     liftM (map DBind bs' ++) (dsDecls ds2)
  where (ds1,ds2)               = span isDBind ds
        bs                      = b : [ b' | DBind b' <- ds1 ]


dsConstr (Constr c ts ps)       = Constr c (map dsQualType ts) (map dsQual ps)

dsSig (Sig vs t)                = Sig vs (dsQualType t)

dsDefault (Default t a b)         = Default t (dsInst a) (dsInst b)

dsInst (Inst v t)               = Inst v (dsQualType t)

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
dsType t                        = internalError "Bad type expressionin dsType" t


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
dsWildType t                    = internalError "Bad type expression in dsWildType" t


-- Base types -------------------------------------------------------------

dsQualBaseType (TQual t ps)     = checkQual (dsBaseType t) (map dsKindQual ps)
dsQualBaseType t                = TQual (dsBaseType t) []

dsBaseType (TAp t t')           = TAp (dsBaseType t) (dsType t')
dsBaseType (TList t)            = TAp (TCon (prim LIST)) (dsType t)
dsBaseType (TCon c)             = TCon c
dsBaseType t                    = internalError "Bad base type expression in dsBaseType" t


-- Predicates ------------------------------------------------------------

dsQual (PKind v k)              = PKind v k
dsQual (PType (TVar v))         = PKind v KWild
dsQual (PType t)                = PType (dsQualPred t)


dsKindQual (PKind v k)          = PKind v k
dsKindQual (PType (TVar v))     = PKind v KWild
dsKindQual q                    = internalError "Bad qualifier in dsKindQual" q


dsQualPred (TQual t ps)         = checkQual (dsSubOrClassPred t) (map dsQual ps)
dsQualPred t                    = TQual (dsSubOrClassPred t) []

dsSubOrClassPred (TSub t t')    = TSub (dsType t) (dsType t')
dsSubOrClassPred t              = dsClassPred t

dsClassPred (TAp t t')          = TAp (dsClassPred t) (dsType t')
dsClassPred (TCon c)            = TCon c
dsClassPred p                   = internalError "Bad class predicate in dsClassPred" p


-- Bindings ---------------------------------------------------------------

dsBinds []                      = return []
dsBinds (BSig vs qt : bs)       = do bs <- dsBinds bs
                                     return (BSig vs (dsQualWildType qt) : bs)
dsBinds bs                      = f [] bs
  where f eqns (BEqn lh rh :bs) = f ((lh,rh) : eqns) bs
        f eqns bs               = do eqns <- dsEqns eqns
                                     bs <- dsBinds bs
                                     return (map (uncurry BEqn) (reverse eqns) ++ bs)


-- Equations -----------------------------------------------------------------

dsEqns []                       = return []
dsEqns ((LPat (EVar x),r):eqns) = dsEqns ((LFun x [], r):eqns)
dsEqns ((LPat (ERec _ fs),RExp (EVar v)):eqns)
                                = do let sels = map (sel (EVar v)) fs
                                     dsEqns (sels ++ eqns)
  where sel e0 (Field n p)      = (LPat p, RExp (ESelect e0 n))
dsEqns ((LPat p,RExp (EVar v)):eqns)
                                = do p' <- dsPat p      -- Check validity
                                     sels <- mapM (sel (EVar v)) vs
                                     dsEqns (sels ++ eqns)
  where vs                      = pvars p
        sel e0 v                = do es <- mapM (const (newEVar paramSym)) vs
                                     return (LFun v [], RExp (selectFrom e0 (vs `zip` es) p v))
dsEqns ((LPat p,rh):eqns)       = do v <- newName tempSym
                                     dsFunBind v [([], rh)] ((LPat p, RExp (EVar v)) : eqns)
dsEqns ((LFun v ps,rh):eqns)    = dsFunBind v [(ps,rh)] eqns


dsFunBind v alts ((LFun v' ps,rh) : eqns)
  | v' == v                     = dsFunBind v ((ps,rh) : alts) eqns
dsFunBind v [(ps,rh)] eqns      = do e <- dsExp (eLam ps (rh2exp rh))
                                     eqns <- dsEqns eqns
                                     return ((LFun v [], RExp e) : eqns)
dsFunBind v alts eqns
  | length arities /= 1         = errorIds "Different arities for function" [v]
  | otherwise                   = do ws <- newNames paramSym (head arities)
                                     alts <- mapM dsA alts
                                     e <- pmc' ws alts
                                     eqns <- dsEqns eqns 
                                     return ((LFun v [], RExp (ELam (map EVar ws) e)) : eqns)
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
rh2exp (RGrd gs)                = ECase (ETup []) [Alt EWild (RGrd gs)]


selectFrom e0 s p v             = ECase e0 [Alt (subst s p) (RExp (subst s (EVar v)))]


-- Expressions --------------------------------------------------------


dsExp (EAp e e')                = liftM2 EAp (dsExp e) (dsExp e')
dsExp (ESig e qt)               = do x <- newName tempSym
                                     dsExp (ELet [BSig [x] qt, BEqn (LFun x []) (RExp e)] (EVar x))
dsExp (ELam ps e)            
  | all isESigVar ps            = liftM2 ELam (mapM dsPat ps) (dsExp e)
  | otherwise                   = do ps <- mapM dsPat ps
                                     e <- dsExp e
                                     ws <- newNames paramSym (length ps)
                                     e' <- pmc' ws [(ps,RExp e)]
                                     return (ELam (zipSigs ws ps) e')
dsExp (ELet bs e)               = liftM2 ELet (dsBinds bs) (dsExp e)
dsExp (EIf e e1 e2)             = dsExp (ECase e [Alt true  (RExp e1), Alt false (RExp e2)])
dsExp (ESectR e op)             = dsExp (EAp (op2exp op) e)
dsExp (ESectL op e)             = do x <- newEVar paramSym
                                     dsExp (ELam [x] (EAp (EAp (op2exp op) x) e))
dsExp (ECase e alts)            = do e <- dsExp e
                                     alts <- mapM dsA alts
                                     pmc e alts
  where dsA (Alt p rh)          = liftM2 Alt (dsPat p) (dsRh rh)
dsExp (ESelect e s)             = liftM (flip ESelect s) (dsExp e)
dsExp (ESel s)                  = do x <- newName paramSym
                                     return (ELam [EVar x] (ESelect (EVar x) s))
dsExp (EWild)                   = errorTree "Non-pattern use of wildcard variable" EWild
dsExp (EVar v)                  = return (EVar v)
dsExp (ECon c)                  = return (ECon c)
dsExp (ELit l)                  = dsLit l
dsExp (ERec m fs)               = liftM (ERec m) (mapM dsF fs)
  where dsF (Field s e)         = liftM (Field s) (dsExp e)
dsExp (EDo v t ss)              = liftM (EDo v (fmap dsWildType t)) (dsStmts ss)
dsExp (ETempl v t ss)           = liftM (ETempl v (fmap dsWildType t)) (dsStmts ss)
dsExp (EAct v ss)               = liftM (EAct v) (dsStmts ss)
dsExp (EReq v ss)               = liftM (EReq v) (dsStmts ss)
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
comp2exp e (QGen p e' : qs) r   = do f <- newName functionSym
                                     x <- newEVar paramSym
                                     e <- comp2exp e qs (EAp (EVar f) x)
                                     return (ELet (binds f x e) (EAp (EVar f) e'))
  where binds f x e             = [BEqn (LFun f [nil]) (RExp r),
                                   BEqn (LFun f [cons p x]) (RExp e)] ++ dflt f x
        dflt f x | isEVar p     = []
                 | otherwise    = [BEqn (LFun f [cons EWild x]) (RExp (EAp (EVar f) x))]


-- Statements ------------------------------------------------------------

dsStmts []                      = return []
dsStmts (SBind b : ss)          = do bs' <- dsBinds bs
                                     liftM (map SBind bs' ++) (dsStmts ss2)
  where (ss1,ss2)               = span isSBind ss
        bs                      = b : [ b' | SBind b' <- ss1 ]
dsStmts (s@(SAss p e) : ss)
  | p == EWild                  = errorTree "Bad assignment" s
  | isEVar p                    = do e <- dsExp e
                                     ss <- dsStmts ss
                                     return (SAss p e : ss)
  | otherwise                   = do v0 <- newName tempSym
                                     assigns <- mapM (assign (EVar v0)) vs
                                     dsStmts (SBind (BEqn (LFun v0 []) (RExp e)) : assigns ++ ss)
  where vs                      = pvars p
        assign e0 v             = do vs' <- newNames paramSym (length vs)
                                     return (SAss (EVar v) (selectFrom e0 (vs `zip` map EVar vs') p v))
dsStmts [SRet e]                = do e <- dsExp e
                                     return [SRet e]
dsStmts [SExp e]                = do e <- dsExp e
                                     return [SExp e]
dsStmts (SGen p e : ss)
  | isESigVar p                 = do p <- dsInnerPat p
                                     e <- dsExp e
                                     ss <- dsStmts ss
                                     return (SGen p e : ss)
  | otherwise                   = do v <- newEVar tempSym
                                     dsStmts (SGen v e : SBind (BEqn (LPat p) (RExp v)) : ss)
dsStmts (s : _)                 = internalError "Internal error in dsStmts; did not expect" s


-- Alternatives -----------------------------------------------------------------------

dsRh (RExp e)                   = liftM  RExp (dsExp e)
dsRh (RGrd gs)                  = liftM  RGrd (mapM dsGrd gs)
dsRh (RWhere rh bs)             = liftM2 RWhere (dsRh rh) (dsBinds bs)

dsGrd (GExp qs e)               = liftM2 GExp (mapM dsEQual qs) (dsExp e)

dsEQual (QExp e)                = liftM (QGen true) (dsExp e)
dsEQual (QGen p e)              = liftM2 QGen (dsPat p) (dsExp e)
dsEQual (QLet bs)               = liftM  QLet (dsBinds bs)


-- Patterns ----------------------------------------------------------------------------

dsPat (ESig p qt)
  | isEVar p                    = do p <- dsPat p
                                     return (ESig p (dsQualWildType qt))
dsPat (EVar v)                  = return (EVar v)
dsPat (EWild)                   = do v <- newName dummySym
                                     return (EVar v)
dsPat (ENeg (ELit (LInt i)))    = return (ELit (LInt (-i)))
dsPat (ENeg (ELit (LRat r)))    = return (ELit (LRat (-r)))
dsPat (ELit l)                  = dsLit l
dsPat (ETup ps)                 = dsPat (foldl EAp (ECon (tuple (length ps))) ps)
dsPat (EList ps)                = dsPat (foldr cons nil ps)
dsPat p                         = dsConPat p

dsConPat (EAp p p')             = liftM2 EAp (dsConPat p) (dsInnerPat p')
dsConPat (ECon c)               = return (ECon c)
dsConPat p                      = errorTree "Illegal pattern" p

dsInnerPat (ESig p t)
  | isEVar p                    = do p <- dsPat p
                                     return (ESig p (dsWildType t))
dsInnerPat p                    = dsPat p


-- Literals ------------------------------------------------------------------

dsLit (LStr s)                  = return (foldr cons nil (map (ELit . LChr) s))
dsLit l                         = return (ELit l)


-- Primitives ----------------------------------------------------------------

cons x xs                       = EAp (EAp (ECon (prim CONS)) x) xs
nil                             = ECon (prim NIL)
true                            = ECon (prim TRUE)
false                           = ECon (prim FALSE)




