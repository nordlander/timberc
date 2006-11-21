module Desugar(desugar) where

import Monad
import Common
import Syntax
import Match
import Maybe
import Fixity

desugar :: Module -> M Module
desugar m = dsModule m


-- Modules ------------------------------------------------------------------

dsModule :: Module -> M Module
dsModule (Module c ds)          = do ds <- dsDecls ds
                                     return (Module c ds)


-- Top level declarations ---------------------------------------------------



dsDecls []                      = return []
dsDecls (d@(DKSig _ _) : ds)    = liftM (d :) (dsDecls ds)
dsDecls (DData c vs bs cs : ds) = liftM (DData c vs (map dsQualBaseType bs) (map dsConstr cs) :) (dsDecls ds)
dsDecls (DRec i c vs bs ss:ds)  = liftM (DRec i c vs (map dsQualBaseType bs) (map dsSig ss) :) (dsDecls ds)
dsDecls (DType c vs t : ds)     = liftM (DType c vs (dsType t) :) (dsDecls ds)
dsDecls (DInst t bs : ds)       = do w <- newName witnessSym
                                     dsDecls (DPSig w (dsQualPred t) : DBind (BEqn (LFun w []) (RWhere (RExp r) bs)) : ds)
  where r                       = ERec (Just (type2head t,True)) (map mkField (bvars bs))
        mkField v | isId v      = Field (mkSel v) (EVar v)
                  | otherwise   = error "Illegal symbol bindinging in instance declaration"
dsDecls (DPSig v t : ds)        = liftM (DPSig v (dsQualPred t) :) (dsDecls ds)
dsDecls (DBind b : ds)          = do bs' <- dsBinds bs
                                     liftM (map DBind bs' ++) (dsDecls ds2)
  where (ds1,ds2)               = span isDBind ds
        bs                      = b : [ b' | DBind b' <- ds1 ]


dsConstr (Constr c ts ps)       = Constr c (map dsQualType ts) (map dsQual ps)

dsSig (Sig vs t)                = Sig vs (dsQualType t)


-- Types ----------------------------------------------------------------------

dsQualType (TQual t ps)         = checkQual (dsRhoType t) (map dsQual ps)
dsQualType t                    = TQual (dsRhoType t) []

dsRhoType (TFun ts t)           = TFun (map dsQualType ts) (dsRhoType t)
dsRhoType t                     = dsType t

dsType (TAp t t')               = TAp (dsType t) (dsType t')
dsType (TFun ts t)              = TFun (map dsType ts) (dsType t)
dsType (TList t)                = TAp (TCon (prim LIST)) (dsType t)
dsType (TTup ts)                = foldl TAp (TCon (tuple (length ts))) (map dsType ts)
dsType (TCon c)                 = TCon c
dsType (TVar v)                 = TVar v
dsType _                        = error ("Bad type expression")


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
dsWildType _                    = error ("Bad type expression")


-- Base types -------------------------------------------------------------

dsQualBaseType (TQual t ps)     = checkQual (dsBaseType t) (map dsKindQual ps)
dsQualBaseType t                = TQual (dsBaseType t) []

dsBaseType (TAp t t')           = TAp (dsBaseType t) (dsType t')
dsBaseType (TList t)            = TAp (TCon (prim LIST)) (dsType t)
dsBaseType (TCon c)             = TCon c
dsBaseType _                    = error ("Bad base type expression")


-- Predicates ------------------------------------------------------------

dsQual (PKind v k)              = PKind v k
dsQual (PType (TVar v))         = PKind v KWild
dsQual (PType t)                = PType (dsQualPred t)


dsKindQual (PKind v k)          = PKind v k
dsKindQual (PType (TVar v))     = PKind v KWild
dsKindQual _                    = error ("Bad qualifier")


dsQualPred (TQual t ps)         = checkQual (dsSubPred t) (map dsQual ps)
dsQualPred t                    = TQual (dsSubPred t) []

dsSubPred (TSub t t')           = dsType (TFun [t] t')
dsSubPred t                     = dsPred t

dsPred (TAp t t')               = TAp (dsPred t) (dsType t')
dsPred (TCon c)                 = TCon c
dsPred _                        = error ("Bad class predicate")


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

dsEqns                          :: [Eqn] -> M [Eqn]
dsEqns []                       = return []
dsEqns ((LPat p,rh):eqns)       = do v0 <- newName tempSym
                                     sels <- mapM (sel (EVar v0)) vs
                                     dsEqns ((LFun v0 [], RExp (rh2exp rh)) : sels ++ eqns)
  where vs                      = pvars p
        sel e0 v                = do es <- mapM (const (newEVar paramSym)) vs
                                     return (LFun v [], RExp (selectFrom e0 (vs `zip` es) p v))
dsEqns ((LFun v ps,rh):eqns)    = dsFunBind v [(ps,rh)] eqns


dsFunBind v alts ((LFun v' ps,rh) : eqns)
  | v' == v                     = dsFunBind v ((ps,rh) : alts) eqns
dsFunBind v [(ps,rh)] eqns      = do e <- dsExp (ELam ps (rh2exp rh))
                                     eqns <- dsEqns eqns
                                     return ((LFun v [], RExp e) : eqns)
dsFunBind v alts eqns
  | length arities /= 1         = fail ("Different arities for function " ++ show v)
  | otherwise                   = do ws <- newNames paramSym (head arities)
                                     alts <- mapM dsA alts
                                     e <- pmc' ws alts
                                     eqns <- dsEqns eqns 
                                     return ((LFun v [], RExp (ELam (map EVar ws) e)) : eqns)
  where arities                 = nub (map (length . fst) alts)
        dsA (ps,rh)             = liftM2 (,) (mapM dsPat ps) (dsRh rh)


-- Helper functions --------------------------------------------------

checkQual t ps
  | not (null ambig)            = error ("Ambiguous type scheme, orphans: " ++ showids ambig)
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
                                     return (ELam (zipSigs ws ps) e)
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
dsExp (ESel s)                  = return (ESel s)
dsExp (EWild)                   = fail "Illegal expression syntax"
dsExp (EVar v)                  = return (EVar v)
dsExp (ECon c)                  = return (ECon c)
dsExp (ENeg (ELit (LInt i)))    = return (ELit (LInt (-i)))
dsExp (ENeg (ELit (LRat r)))    = return (ELit (LRat (-r)))
--dsExp (ENeg e)                        = dsExp (EAp (EVar Prim.negate) e)
dsExp (ELit l)                  = return (ELit l)
dsExp (ERec m fs)               = liftM (ERec m) (mapM dsF fs)
  where dsF (Field s e)         = liftM (Field s) (dsExp e)
dsExp (EDo ss)                  = liftM EDo (dsStmts ss)
dsExp (ETempl v ss)             = liftM (ETempl v) (dsStmts ss)
dsExp (EAct v ss)               = liftM (EAct v) (dsStmts ss)
dsExp (EReq v ss)               = liftM (EReq v) (dsStmts ss)
dsExp (EAfter e e')             = dsExp (EAp (EAp (EVar (prim After)) e) e')
dsExp (EBefore e e')            = dsExp (EAp (EAp (EVar (prim Before)) e) e')
dsExp (ETup es)                 = dsExp (foldl EAp (ECon (tuple (length es))) es)
dsExp (EList es)                = dsExp (foldr cons nil es)
dsExp (ESeq _ _ _)              = fail "Sequences not yet implemented"
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
dsStmts (SAss p e : ss)
  | isESigVar p                 = do e <- dsExp e
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
dsStmts (SRet e : ss)           = fail "Illegal return"
dsStmts [SExp e]                = do e <- dsExp e
                                     return [SExp e]
dsStmts (SExp e : ss)           = dsStmts (SGen EWild e : ss)
dsStmts (SGen p e : ss)
  | isESigVar p                 = do e <- dsExp e
                                     ss <- dsStmts ss
                                     return (SGen p e : ss)
  | otherwise                   = do v <- newEVar tempSym
                                     dsStmts (SGen v e : SBind (BEqn (LPat p) (RExp v)) : ss)
dsStmts (SCase e as : ss)       = dsStmts (SExp (ECase e (map doAlt as)) : ss)
  where doAlt (Alt p r)         = Alt p (doRhs r)
        doRhs (RExp ss)         = RExp (EDo ss)
        doRhs (RGrd gs)         = RGrd (map doGrd gs)
        doRhs (RWhere r bs)     = RWhere (doRhs r) bs
        doGrd (GExp qs ss)      = GExp qs (EDo ss)
dsStmts (SIf e ss' : ss)        = dsIf (EIf e (EDo ss')) ss
dsStmts (SElsif _ _ : ss)       = fail "Illegal elsif"
dsStmts (SElse _ : ss)          = fail "Illegal else"
dsStmts _                       = fail "Statement not yet implememted"


dsIf f (SElsif e ss' : ss)      = dsIf (f . EIf e (EDo ss')) ss
dsIf f (SElse ss' : ss)         = dsStmts (SExp (f (EDo ss')) : ss)
dsIf f ss                       = dsStmts (SExp (f done) : ss)


-- Alternatives -----------------------------------------------------------------------

dsRh (RExp e)                   = liftM  RExp (dsExp e)
dsRh (RGrd gs)                  = liftM  RGrd (mapM dsGrd gs)
dsRh (RWhere rh bs)             = liftM2 RWhere (dsRh rh) (dsBinds bs)

dsGrd (GExp qs e)               = liftM2 GExp (mapM dsEQual qs) (dsExp e)

dsEQual (QExp e)                = liftM (QGen true) (dsExp e)
dsEQual (QGen p e)              = liftM2 QGen (dsPat p) (dsExp e)
dsEQual (QLet bs)               = liftM  QLet (dsBinds bs)


-- Patterns ----------------------------------------------------------------------------

dsPat (ESig (EVar v) qt)        = return (ESig (EVar v) (dsQualWildType qt))
dsPat (EVar v)                  = return (EVar v)
dsPat (EWild)                   = do v <- newName dummySym
                                     return (EVar v)
dsPat (ENeg (ELit (LInt i)))    = return (ELit (LInt (-i)))
dsPat (ENeg (ELit (LRat r)))    = return (ELit (LRat (-r)))
dsPat (ELit l)                  = return (ELit l)
dsPat (ETup ps)                 = dsPat (foldl EAp (ECon (tuple (length ps))) ps)
dsPat (EList ps)                = dsPat (foldr cons nil ps)
dsPat p                         = dsConPat p

dsConPat (EAp p p')             = liftM2 EAp (dsConPat p) (dsPat p')
dsConPat (ECon c)               = return (ECon c)
dsConPat p                      = fail "Illegal pattern"


-- Primitives ----------------------------------------------------------------

cons x xs                       = EAp (EAp (ECon (prim CONS)) x) xs
nil                             = ECon (prim NIL)
true                            = ECon (prim TRUE)
false                           = ECon (prim FALSE)
done                            = EDo []





