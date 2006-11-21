module Expand where

{-

This module does the following:
  - Checks all binding groups and patterns for duplicates (both types & terms).
  - Checks all signatures for duplicates and dangling references (kind and type sigs).
  - Completes all type signatures and constructor definitions with explicit type variable bindings.
  - Shuffles explicit signatures as close as possible to their binding occurrences (even inside patterns)
  - Checks that variables bound in generators and equations in commands do not shadow state variables.
  - Checks that assignments are to declared state variables only.
  - Resolves proper binding occurrences for all identifier references by setting the unique tag in each Name 
    identifier, or possibly replacing a Name with a Prim.
-}

import Monad
import Common
import Syntax
import List(sort)

expandM m                          = expand initEnv m


-- Syntax traversal environment --------------------------------------------------

type ExpandEnv                     = (Map Name Name, Map Name Name, Map Name Name)

initEnv                            = (primTerms, primTypes, [])


stateVars (_,_,rS)                 = dom rS

tscope (_,rT,_)                    = dom rT


renE _ n@(Tuple _ _)               = n
renE (rE,_,_) v                    = case lookup v rE of
                                       Just n  -> n { annot = annot v }
                                       Nothing -> error ("Undefined identifier: " ++ show v)

renT _ n@(Tuple _ _)               = n
renT (_,rT,_) v                    = case lookup v rT of
                                       Just n  -> n { annot = annot v }
                                       Nothing -> error ("Undefined type identifier: " ++ show v)

renaming vs                        = do ns <- mapM (const newNum) vs
                                        return (zipWith f vs ns)
  where f v n                      = (v, v { tag = n })

extRenE (rE,rT,rS) vs              = do rE' <- renaming (noDups vs)
                                        return (rE'++rE,rT,rS)

extRenS (rE,rT,rS) vs              = do rS' <- renaming (noDups vs)
                                        return (rS'++rE, rT, rS')

extRenT (rE,rT,rS) vs              = do rT' <- renaming (noDups vs)
                                        return (rE,rT'++rT,rS)

extCurrent (rE,rT,rS)              = (primCurrent++rE,rT,rS)

reopenState (rE,rT,rS)             = (rS++rE,rT,rS)

noDups vs
  | not (null dups)                = error ("Duplicate variables: " ++ showids dups)
  | otherwise                      = vs
  where dups                       = duplicates vs


-- Binding of overloaded names ------------------------------------------------------------------------

oloadBinds xs ds                   = concat [ binds c vs sels | DRec True c vs _ sels <- ds ]
  where binds c vs sels            = concat [ binds' s x t | Sig ss t <- sels, s <- ss, let x = chopSel s, x `notElem` xs ]
          where p0                 = PType (foldl TAp (TCon c) (map TVar vs))
                binds' x s t       = [ BSig [x] (tQual [p0] t), 
                                       BEqn (LFun (annotExplicit x) [w]) (RExp (ESelect w s)) ]
        w                          = EVar (name0 paramSym)


-- Renaming -----------------------------------------------------------------------------------------

class Expand a where
  expand :: ExpandEnv -> a -> M a

instance Expand a => Expand [a] where
  expand env as                    = mapM (expand env) as

instance Expand a => Expand (Maybe a) where
  expand env (Just e)              = liftM Just (expand env e)
  expand env Nothing               = return Nothing


instance Expand Module where
  expand env (Module c ds)         = do env1 <- extRenT env ts
                                        env2 <- extRenE env1 (ss++cs++vs)
                                        liftM (Module c) (expand env2 ds')
    where (ts,ss,cs,bs)            = expD [] [] [] [] [] [] ds
          ds'                      = filter (not . isDBind) ds ++ map DBind (bs' ++ bs)
          vs                       = bvars bs
          bs'                      = oloadBinds vs ds


expD ks ts ss cs ws bs (DKSig c k : ds)
  | c `elem` ks                    = error ("Duplicated kind signature: " ++ show c)
  | otherwise                      = expD (c:ks) ts ss cs ws bs ds
expD ks ts ss cs ws bs (DRec _ c _ _ sigs : ds)
  | c `elem` ts                    = error ("Duplicated type constructor definition: " ++ show c)
  | not (null dups)                = error ("Duplicated selectors: " ++ showids dups)
  | otherwise                      = expD (ks\\[c]) (c:ts) (sels++ss) cs ws bs ds
  where sels                       = concat [ vs | Sig vs t <- sigs ]
        dups                       = duplicates sels ++ (ss `intersect` sels)
expD ks ts ss cs ws bs (DData c _ _ cdefs : ds)
  | c `elem` ts                    = error ("Duplicated type constructor definition: " ++ show c)
  | not (null dups)                = error ("Duplicated constructors: " ++ showids dups)
  | otherwise                      = expD (ks\\[c]) (c:ts) ss (cons++cs) ws bs ds
  where cons                       = [ c | Constr c _ _ <- cdefs ]
        dups                       = duplicates cons ++ (cs `intersect` cons)
expD ks ts ss cs ws bs (DInst _ _ : ds)
                                   = expD ks ts ss cs ws bs ds
expD ks ts ss cs ws bs (DType c _ _ : ds)
  | c `elem` ts                    = error ("Duplicated type constructor definition: " ++ show c)
  | otherwise                      = expD (ks\\[c]) (c:ts) ss cs ws bs ds
expD ks ts ss cs ws bs (DPSig v t : ds)
  | v `elem` ws                    = error ("Duplicated instance signature: " ++ show v)
  | otherwise                      = expD ks ts ss cs (v:ws) bs ds
expD ks ts ss cs ws bs (DBind b : ds)
                                   = expD ks ts ss cs ws (b:bs) ds
expD ks ts ss cs ws bs []
  | not (null ks)                  = error ("Dangling kind signatures: " ++ showids ks)
  | otherwise                      = (ts, ss, cs, shuffleD ws (reverse bs))


instance Expand Decl where
  expand env d@(DKSig _ _)         = return d
  expand env (DData c vs ts cs)    = do env' <- extRenT env vs
                                        liftM2 (DData (renT env c) (map (renT env') vs)) (expandQTs env' ts) (expand env' cs)
  expand env (DRec isC c vs ts ss) = do env' <- extRenT env vs
                                        liftM2 (DRec isC (renT env c) (map (renT env') vs)) (expandQTs env' ts) (expand env' ss)
  expand env (DType c vs t)        = do env' <- extRenT env vs
                                        liftM (DType (renT env c) (map (renT env') vs)) (expand env' t)
  expand env (DInst t bs)          = liftM2 DInst (expandQT env t) (expand env bs)
  expand env (DPSig v t)           = liftM (DPSig (renE env v)) (expandQT env t)
  expand env (DBind b)             = liftM DBind (expand env b)

instance Expand Constr where
  expand env (Constr c ts ps)      = do env' <- extRenT env (bvars ps')
                                        liftM2 (Constr (renE env c)) (expand env' ts) (expand env' ps')
   where ps'                       = completeP env ts ps

completeP env t ps
  | not (null dups)                = error ("Duplicate type variable abstractions: " ++ showids dups)
  | not (null dang)                = error ("Dangling type variable abstractions: " ++ showids dang)
  | otherwise                      = ps ++ zipWith PKind implicit (repeat KWild)
  where vs                         = tyvars t ++ tyvars ps
        bvs                        = bvars ps
        dups                       = duplicates bvs
        dang                       = bvs \\ vs
        implicit                   = nub vs \\ (tscope env ++ bvs)


expandQT env (TQual t ps)          = expand env (TQual t (completeP env t ps))
expandQT env t                     = expandQT env (TQual t [])


expandQTs env ts                   = mapM (expandQT env) ts


instance Expand Sig where
  expand env (Sig vs t)            = liftM (Sig (map (renE env) vs)) (expandQT env t)

instance Expand Pred where
  expand env (PType p)             = liftM PType (expand env p)
  expand env (PKind v k)           = return (PKind (renT env v) k)

instance Expand Type where
  expand env (TQual t ps)          = do env' <- extRenT env (bvars ps)
                                        liftM2 TQual (expand env' t) (expand env' ps)
  expand env (TCon c)              = return (TCon (renT env c))
  expand env (TVar v)              = return (TVar (renT env v))
  expand env (TAp t1 t2)           = liftM2 TAp (expand env t1) (expand env t2)
  expand env (TSub t1 t2)          = liftM2 TSub (expand env t1) (expand env t2)
  expand env TWild                 = return TWild
  expand env (TList ts)            = liftM TList (expand env ts)
  expand env (TTup ts)             = liftM TTup (expand env ts)
  expand env (TFun t1 t2)          = liftM2 TFun (expand env t1) (expand env t2)

instance Expand Bind where
  expand env (BEqn lh rh)          = do env' <- extRenE env (bvars lh)
                                        liftM2 BEqn (expand env' lh) (expand env' rh)
  expand env (BSig vs t)           = return (BSig (map (renE env) vs) t)

instance Expand Lhs where
  expand env (LFun v ps)           = liftM (LFun (renE env v)) (expand env ps)
  expand env (LPat p)              = liftM LPat (expand env p)

instance Expand Exp where
  expand env (EVar v)              = return (EVar (renE env v))
  expand env (ECon c)              = return (ECon (renE env c))
  expand env (ESel l)              = return (ESel (renE env l))
  expand env (EAp e1 e2)           = liftM2 EAp (expand env e1) (expand env e2)
  expand env (ELit l)              = return (ELit l)
  expand env (ETup ps)             = liftM ETup (expand env ps)
  expand env (EList es)            = liftM EList (expand env es)
  expand env EWild                 = return EWild
  expand env (ESig e t)            = liftM2 ESig (expand env e) (expandQT env t)
  expand env (ERec m fs)           = liftM (ERec m) (expand env fs)
  expand env (ELam ps e)           = do env' <- extRenE env (pvars ps)
                                        liftM2 ELam (expand env' ps) (expand env' e)
  expand env (ELet bs e)           = do env' <- extRenE env (bvars bs)
                                        liftM2 ELet (expand env' (shuffleB bs)) (expand env' e)
  expand env (ECase e as)          = liftM2 ECase (expand env e) (expand env as)
  expand env (EIf e1 e2 e3)        = liftM3 EIf (expand env e1) (expand env e2) (expand env e3)
  expand env (ENeg e)              = liftM ENeg (expand env e)
  expand env (ESeq e1 e2 e3)       = liftM3 ESeq (expand env e1) (expand env e2) (expand env e3)
  expand env (EComp e qs)          = liftM2 EComp (expand env e) (expQ env qs)
  expand env (ESectR e op)         = liftM (flip ESectR op) (expand env e) 
  expand env (ESectL op e)         = liftM (ESectL op) (expand env e)
  expand env (ESelect e l)         = liftM (flip ESelect (renE env l)) (expand env e) 
  expand env (EDo ss)              = liftM EDo (expS (reopenState (extCurrent env)) (shuffleS ss))
  expand env (EAct v ss)           = liftM (EAct (renE env v)) (expS (reopenState (extCurrent env)) (shuffleS ss))
  expand env (EReq v ss)           = liftM (EReq (renE env v)) (expS (reopenState (extCurrent env)) (shuffleS ss))
  expand env (ETempl v ss)
    | not (null shadowed)          = error ("Illegal shadowing of state variable: " ++ showids shadowed)
    | otherwise                    = do env1 <- extRenS env st
                                        env2 <- extRenE env1 vs
                                        liftM (ETempl (renE env2 v)) (expand env2 (shuffleS ss))
    where st                       = svars ss
          vs                       = v : bvars ss
          shadowed                 = intersect st vs
  expand env (EAfter e1 e2)        = liftM2 EAfter (expand env e1) (expand env e2)
  expand env (EBefore e1 e2)       = liftM2 EBefore (expand env e1) (expand env e2)


instance Expand Field where
  expand env (Field l e)           = liftM (Field (renE env l)) (expand env e)

instance Expand (Rhs Exp) where
  expand env (RExp e)              = liftM RExp (expand env e)
  expand env (RGrd gs)             = liftM RGrd (expand env gs)
  expand env (RWhere e bs)         = do env' <- extRenE env (bvars bs)
                                        liftM2 RWhere (expand env' e) (expand env' (shuffleB bs))

instance Expand (Rhs [Stmt]) where
  expand env (RExp ss)             = liftM RExp (expand env (shuffleS ss))
  expand env (RGrd gs)             = liftM RGrd (expand env gs)
  expand env (RWhere e bs)         = do env' <- extRenE env (bvars bs)
                                        liftM2 RWhere (expand env' e) (expand env' (shuffleB bs))


instance Expand (GExp Exp) where
  expand env (GExp qs e)           = liftM2 GExp (expQ env qs) (expand env e)

instance Expand (GExp [Stmt]) where
  expand env (GExp qs ss)          = liftM2 GExp (expQ env qs) (expS env (shuffleS ss))

instance Expand (Alt Exp) where
  expand env (Alt  p rh)           = do env' <- extRenE env (pvars p)
                                        liftM2 Alt (expand env' p) (expand env' rh) 

instance Expand (Alt [Stmt]) where
  expand env (Alt p rh)            = do env' <- extRenE env (pvars p)
                                        liftM2 Alt (expand env' p) (expand env' rh) 

instance Expand Qual where
  expand env (QExp e)              = liftM QExp (expand env e)
  expand env (QGen p e)            = liftM2 QGen (expand env p) (expand env e)
  expand env (QLet bs)             = liftM QLet (expand env (shuffleB bs))


instance Expand Stmt where
  expand env (SExp e)              = liftM SExp (expand env e)
  expand env (SRet e)              = liftM SRet (expand env e)
  expand env (SGen p e)            = liftM2 SGen (expand env p) (expand env e)
  expand env (SBind b)             = liftM SBind (expand env b)
  expand env (SAss p e)
    | not (null illegal)           = fail ("Unknown state variable: " ++ showids illegal)
    | otherwise                    = liftM2 SAss (expand env p) (expand env e)
    where illegal                  = pvars p \\ stateVars env
  expand env (SForall qs ss)       = liftM2 SForall (expQ env qs) (expS env (shuffleS ss))
  expand env (SWhile e ss)         = liftM2 SWhile (expand env e) (expS env (shuffleS ss))
  expand env (SIf e ss)            = liftM2 SIf (expand env e) (expS env (shuffleS ss))
  expand env (SElsif e ss)         = liftM2 SElsif (expand env e) (expS env (shuffleS ss))
  expand env (SElse ss)            = liftM SElse (expS env (shuffleS ss))
  expand env (SCase e as)          = liftM2 SCase (expand env e) (expand env as)


expS env []                        = return []
expS env (SGen p e : ss)         
  | not (null shadowed)            = fail ("Illegal shadowing of state variable: " ++ showids shadowed)
  | otherwise                      = do env' <- extRenE env vs
                                        stmt <- liftM2 SGen (expand env' p) (expand env e)
                                        liftM (stmt:) (expS env' ss)
  where vs                         = pvars p
        shadowed                   = intersect vs (stateVars env)
expS env ss@(SBind _ : _)
  | not (null shadowed)            = fail ("Illegal shadowing of state variable: " ++ showids shadowed)
  | otherwise                      = do env' <- extRenE env vs
                                        liftM2 (++) (expand env' ss1) (expS env' ss2)
  where (ss1,ss2)                  = span isSBind ss
        vs                         = bvars ss1
        shadowed                   = intersect vs (stateVars env)
expS env (stmt : ss)               = liftM2 (:) (expand env stmt) (expS env ss)


expQ env []                        = return []
expQ env (q@(QExp _) : qs)         = liftM2 (:) (expand env q) (expQ env qs)
expQ env (q@(QGen p _) : qs)       = do env' <- extRenE env (pvars p)
                                        liftM2 (:) (expand env' q) (expQ env' qs)
expQ env (q@(QLet bs) : qs)        = do env' <- extRenE env (bvars bs)
                                        liftM2 (:) (expand env' q) (expQ env' qs)


-- Signature shuffling -------------------------------------------------------------------------

shuffleD insts bs                  = shuffle insts [] bs

shuffleB bs                        = shuffle [] [] bs

shuffleS ss                        = shuffle' [] ss


shuffle [] [] []                   = []
shuffle insts [] []                = error ("Dangling instance signatures: " ++ showids insts)
shuffle [] sigs []                 = error ("Dangling type signatures: " ++ showids (dom sigs))
shuffle insts sigs (BSig vs t : bs)
  | not (null s_dups)              = error ("Duplicate type signatures: " ++ showids s_dups)
  | not (null i_dups)              = error ("Signatures overlap with instances: " ++ showids i_dups)
  | otherwise                      = shuffle insts (vs `zip` repeat t ++ sigs) bs
  where s_dups                     = duplicates vs ++ (vs `intersect` dom sigs)
        i_dups                     = vs `intersect` insts
shuffle insts sigs (b@(BEqn (LFun v _) _) : bs)
                                   = case lookup v sigs of
                                       Just t  -> BSig [v] t : b : shuffle insts (prune sigs [v]) bs
                                       Nothing -> b : shuffle (insts \\ [v]) sigs bs
shuffle insts sigs (BEqn (LPat p) rh : bs)
  | not (null illegal)             = error ("Illegal instance binding for: " ++ showids illegal)
  | otherwise                      = BEqn (LPat p') rh : shuffle insts sigs' bs
  where illegal                    = pvars p `intersect` insts
        (sigs',p')                 = attach sigs p



shuffle' [] []                     = []
shuffle' sigs []                   = error ("Dangling type signatures for: " ++ showids (dom sigs))
shuffle' sigs (SBind (BSig vs t) : ss)
  | not (null dups)                = error ("Multiple type signatures for: " ++ showids dups)
  | otherwise                      = shuffle' (vs `zip` repeat t ++ sigs) ss
  where dups                       = duplicates vs ++ (vs `intersect` dom sigs)
shuffle' sigs (s@(SBind (BEqn (LFun v ps) rh)) : ss)
                                   = case lookup v sigs of
                                       Just t  -> SBind (BSig [v] t) : s : shuffle' (prune sigs [v]) ss
                                       Nothing -> s : shuffle' sigs ss
shuffle' sigs (SBind (BEqn (LPat p) rh) : ss)
                                   = SBind (BEqn (LPat p') rh) : shuffle' sigs' ss
  where (sigs',p')                 = attach sigs p
shuffle' sigs (SGen p e : ss)      = SGen p' e : shuffle' sigs' ss
  where (sigs',p')                 = attach sigs p
shuffle' sigs (SAss p e : ss)      = SAss p' e : shuffle' sigs' ss
  where (sigs',p')                 = attach sigs p
shuffle' sigs (s : ss)             = s : shuffle' sigs ss



attach sigs e@(ESig (EVar v) t)
  | v `elem` dom sigs              = error ("Conflicting signatures for " ++ show v)
  | otherwise                      = (sigs, e)
attach sigs (EVar v)               = case lookup v sigs of
                                       Nothing -> (sigs, EVar v)
                                       Just t  -> (prune sigs [v], ESig (EVar v) t)
attach sigs (EAp p1 p2)            = (sigs2, EAp p1' p2')
  where (sigs1,p1')                = attach sigs p1
        (sigs2,p2')                = attach sigs1 p2
attach sigs (ETup ps)              = (sigs', ETup ps')
  where (sigs',ps')                = attachList sigs ps
attach sigs (EList ps)             = (sigs', EList ps')
  where (sigs',ps')                = attachList sigs ps
attach sigs p                      = (sigs, p)

attachList sigs []                 = (sigs, [])
attachList sigs (p:ps)             = (sigs2, p':ps')
  where (sigs1,p')                 = attach sigs p
        (sigs2,ps')                = attachList sigs1 ps


