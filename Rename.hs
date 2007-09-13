module Rename where

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

renameM e1 m                       = rename (initEnv e1) m
                                

-- Syntax traversal environment --------------------------------------------------

data Env                           = Env { rE :: Map Name Name, 
                                           rT :: Map Name Name, 
                                           rS :: Map Name Name,
                                           rL :: Map Name Name,
                                           self :: [Name],
                                           void :: [Name]
                                         } deriving Show

initEnv (rL',rT',rE')              = Env { rE = primTerms ++ rE', rT = primTypes ++ rT', rS = [], rL = rL', self = [], void = [] }

stateVars env                      = dom (rS env)

tscope env                         = dom (rT env)


renE env n@(Tuple _ _)             = n
renE env v                         = case lookup v (rE env) of
                                       Just n  -> n { annot = annot v }
                                       Nothing -> error ("Undefined identifier: " ++ show v)

renS env n@(Tuple _ _)             = n
renS env v                         = case lookup v (rS env) of
                                       Just n  -> n { annot = a { stateVar = True} }
                                       Nothing -> error ("Undefined state variable: " ++ show v)
  where a                          = annot v

renL env v                         = case lookup v (rL env) of
                                       Just n  -> n { annot = annot v }
                                       Nothing -> error ("Undefined selector: " ++ show v)

renT env n@(Tuple _ _)             = n
renT env v                         = case lookup v (rT env) of
                                       Just n  -> n { annot = annot v }
                                       Nothing -> error ("Undefined type identifier: " ++ show v)

extRenE env vs
  | not (null shadowed)            = fail ("Illegal shadowing of state variable: " ++ showids shadowed)
  | not (null shadowed')           = fail ("Illegal shadowing of state reference: " ++ showids shadowed')
  | otherwise                      = do rE' <- renaming (noDups vs)
                                        return (env { rE = rE' ++ rE env })
  where shadowed                   = intersect vs (stateVars env)
        shadowed'                  = intersect vs (self env)


setRenS env vs
  | not (null shadowed)            = fail ("Illegal shadowing of state reference: " ++ showids shadowed)
  | otherwise                      = do rS' <- renaming (noDups vs)
                                        return (env { rS = rS', void = vs })
  where shadowed                   = intersect vs (self env)

unvoid vs env                      = env { void = void env \\ vs }

unvoidAll env                      = env { void = [] }

extRenT env vs                     = do rT' <- renaming (noDups vs)
                                        return (env { rT = rT' ++ rT env })
extRenEMod pub m env vs            = do rE' <- extRenXMod pub m (rE env) vs
                                        return (env {rE = rE'})

extRenTMod pub m env vs            = do rT' <- extRenXMod pub m (rT env) vs
                                        return (env {rT = rT'})

extRenLMod pub m env vs            = do rL' <- extRenXMod pub m (rL env) vs
                                        return (env {rL = rL'})

extRenSelf env s                   = do rE' <- renaming [s]
                                        return (env { rE = rE' ++ rE env, self = [s] })

extRenXMod pub m rX vs             = do rX' <- renaming (map (mName (qual pub)) (noQual vs))
                                        let rX'' = if pub 
                                                   then (mergeRenamings1 (mapFst (mName Nothing) rX') rX) ++ rX'
                                                   else mergeRenamings1 rX' rX
                                        return (rX'') 
  where qual True                  = Just (str m)
        qual False                 = Nothing

noQual vs                          = map checkQual vs
  where checkQual v
         | fromMod v == Nothing    = v
         | otherwise               = error ("Binding occurrence may not be qualified: " ++ show v)

-- Binding of overloaded names ------------------------------------------------------------------------

oloadBinds xs ds                   = concat [ binds c vs sels | DRec True c vs _ sels <- ds ]
  where binds c vs sels            = concat [ binds' s t | Sig ss t <- sels, s <- ss, s `notElem` xs ]
          where p0                 = PType (foldl TAp (TCon c) (map TVar vs))
                binds' s t         = [ BSig [s] (tQual [p0] t),
                                       BEqn (LFun (annotExplicit s) [w]) (RExp (ESelect w s)) ]
        w                          = EVar (name0 paramSym)


-- Renaming -----------------------------------------------------------------------------------------

class Rename a where
  rename :: Env -> a -> M s a

instance Rename a => Rename [a] where
  rename env as                    = mapM (rename env) as

instance Rename a => Rename (Maybe a) where
  rename env (Just e)              = liftM Just (rename env e)
  rename env Nothing               = return Nothing


instance Rename Module where
  rename env (Module c is ds ps)   = do assert (null kDups) ("Duplicate kind signatures " ++ showids kDups)
                                        assert (null tDups) ("Duplicate type constructors " ++ showids tDups)
                                        assert (null sDups) ("Duplicate selectors " ++ showids sDups)
                                        assert (null cDups) ("Duplicate constructors " ++ showids cDups)
                                        assert (null wDups) ("Duplicate instance signatures " ++ showids wDups)
                                        assert (null eDups) ("Duplicate top-level variable " ++ showids eDups)
                                        assert (null dks)   ("Dangling kind signatures " ++ showids dks)
                                        env1 <- extRenTMod True  c env  (ts1 ++ ks1')
                                        env2 <- extRenEMod True  c env1 (cs1 ++ vs1 ++ vs1' ++ vss)
                                        env3 <- extRenLMod True  c env2 ss1
                                        env4 <- extRenTMod False c env3 ((ts2 \\ ks1') ++ ks2)
                                        env5 <- extRenEMod False c env4 (cs2 ++ (vs2 \\ vss) ++ vs2')
                                        env6 <- extRenLMod False c env5 ss2
                                        let bs = shuffleD ws (bs1 ++ bs2)
                                        ds' <- rename env6 (filter (not . isDBind) (ds ++ ps) ++ map DBind (bs1' ++ bs2' ++ bs))
                                        return (Module c is ds' [])
   where (ks1,ts1,ss1,cs1,ws1,bs1) = renameD [] [] [] [] [] [] ds
         (ks2,ts2,ss2,cs2,ws2,bs2) = renameD [] [] [] [] [] [] ps
         vs1                       = bvars bs1
         vs2                       = bvars bs2
         vss                       = concat [ ss | BSig ss _ <- bs1 ] \\ vs1
         vs                        = vs1 ++ vs2
         ks                        = ks1 ++ ks2
         ts                        = ts1 ++ ts2
         ws                        = ws1 ++ ws2
         bs1'                      = oloadBinds vs ds
         bs2'                      = oloadBinds vs ps
         vs1'                      = bvars bs1'
         vs2'                      = bvars bs2'
         ks1'                      = ks1 \\ ts1
         dks                       = ks \\ ts
         kDups                     = duplicates ks
         tDups                     = duplicates ts
         sDups                     = duplicates (ss1 ++ ss2)
         cDups                     = duplicates (cs1 ++ cs2)
         wDups                     = duplicates ws
         eDups                     = duplicates vs
         
renameD ks ts ss cs ws bs (DKSig c k : ds)
                                   = renameD (c:ks) ts ss cs ws bs ds
renameD ks ts ss cs ws bs (DRec _ c _ _ sigs : ds)
                                   = renameD ks (c:ts) (sels++ss) cs ws bs ds
  where sels                       = concat [ vs | Sig vs t <- sigs ]
renameD ks ts ss cs ws bs (DData c _ _ cdefs : ds)
                                   = renameD ks (c:ts) ss (cons++cs) ws bs ds
  where cons                       = [ c | Constr c _ _ <- cdefs ]
renameD ks ts ss cs ws bs (DInst _ _ : ds)
                                   = renameD ks ts ss cs ws bs ds
renameD ks ts ss cs ws bs (DType c _ _ : ds)
                                   = renameD ks (c:ts) ss cs ws bs ds
renameD ks ts ss cs ws bs (DPSig v t : ds)
                                   = renameD ks ts ss cs (v:ws) bs ds
renameD ks ts ss cs ws bs (DDefault _ : ds)
                                   = renameD ks ts ss cs ws bs ds
renameD ks ts ss cs ws bs (DBind b : ds)
                                   = renameD ks ts ss cs ws (b:bs) ds
renameD ks ts ss cs ws bs []       = (ks, ts, ss, cs, ws, reverse bs)


instance Rename Decl where
  rename env d@(DKSig _ _)         = return d
  rename env (DData c vs ts cs)    = do env' <- extRenT env vs
                                        liftM2 (DData (renT env c) (map (renT env') vs)) (renameQTs env' ts) (rename env' cs)
  rename env (DRec isC c vs ts ss) = do env' <- extRenT env vs
                                        liftM2 (DRec isC (renT env c) (map (renT env') vs)) (renameQTs env' ts) (rename env' ss)
  rename env (DType c vs t)        = do env' <- extRenT env vs
                                        liftM (DType (renT env c) (map (renT env') vs)) (rename env' t)
  rename env (DInst t bs)          = liftM2 DInst (renameQT env t) (rename env bs)
  rename env (DPSig v t)           = liftM (DPSig (renE env v)) (renameQT env t)
  rename env (DDefault ts)         = liftM DDefault (rename env ts)
  rename env (DBind b)             = liftM DBind (rename env b)

instance Rename Default where
  rename env (Default a b)         = liftM2 Default (rename env a) (rename env b)

instance Rename Inst where
  rename env (Inst Nothing t)      = liftM (Inst Nothing) (renameQT env t)
  rename env (Inst (Just v) t)     = liftM (Inst (Just (renE env v))) (renameQT env t)

instance Rename Constr where
  rename env (Constr c ts ps)      = do env' <- extRenT env (bvars ps')
                                        liftM2 (Constr (renE env c)) (rename env' ts) (rename env' ps')
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


renameQT env (TQual t ps)          = rename env (TQual t (completeP env t ps))
renameQT env t                     = renameQT env (TQual t [])


renameQTs env ts                   = mapM (renameQT env) ts


instance Rename Sig where
  rename env (Sig vs t)            = liftM (Sig (map (renL env) vs)) (renameQT env t)

instance Rename Pred where
  rename env (PType p)             = liftM PType (rename env p)
  rename env (PKind v k)           = return (PKind (renT env v) k)

instance Rename Type where
  rename env (TQual t ps)          = do env' <- extRenT env (bvars ps)
                                        liftM2 TQual (rename env' t) (rename env' ps)
  rename env (TCon c)              = return (TCon (renT env c))
  rename env (TVar v)              = return (TVar (renT env v))
  rename env (TAp t1 t2)           = liftM2 TAp (rename env t1) (rename env t2)
  rename env (TSub t1 t2)          = liftM2 TSub (rename env t1) (rename env t2)
  rename env TWild                 = return TWild
  rename env (TList ts)            = liftM TList (rename env ts)
  rename env (TTup ts)             = liftM TTup (rename env ts)
  rename env (TFun t1 t2)          = liftM2 TFun (rename env t1) (rename env t2)

instance Rename Bind where
  rename env (BEqn lh rh)          = do env' <- extRenE env (bvars lh)
                                        liftM2 BEqn (rename env' lh) (rename env' rh)
  rename env (BSig vs t)           = liftM (BSig (map (renE env) vs)) (renameQT env t)

instance Rename Lhs where
  rename env (LFun v ps)           = liftM (LFun (renE env v)) (rename env ps)
  rename env (LPat p)              = liftM LPat (rename env p)

instance Rename Exp where
  rename env (EVar v)
    | v `elem` void env            = fail ("Uninitialized state variable: " ++ show v)
    | v `elem` stateVars env       = return (EVar (renS env v))
    | otherwise                    = return (EVar (renE env v))
  rename env (ECon c)              = return (ECon (renE env c))
  rename env (ESel l)              = return (ESel (renL env l))
  rename env (EAp e1 e2)           = liftM2 EAp (rename env e1) (rename env e2)
  rename env (ELit l)              = return (ELit l)
  rename env (ETup ps)             = liftM ETup (rename env ps)
  rename env (EList es)            = liftM EList (rename env es)
  rename env EWild                 = return EWild
  rename env (ESig e t)            = liftM2 ESig (rename env e) (renameQT env t)
  rename env (ERec m fs)           = liftM (ERec (renRec env m)) (rename env fs)
  rename env (ELam ps e)           = do env' <- extRenE env (pvars ps)
                                        liftM2 ELam (rename env' ps) (rename env' e)
  rename env (ELet bs e)           = do env' <- extRenE env (bvars bs)
                                        liftM2 ELet (rename env' (shuffleB bs)) (rename env' e)
  rename env (ECase e as)          = liftM2 ECase (rename env e) (rename env as)
  rename env (EIf e1 e2 e3)        = liftM3 EIf (rename env e1) (rename env e2) (rename env e3)
  rename env (ENeg e)              = liftM ENeg (rename env e)
  rename env (ESeq e1 e2 e3)       = liftM3 ESeq (rename env e1) (rename env e2) (rename env e3)
  rename env (EComp e qs)          = do (qs,e) <- renameQ env qs e
                                        return (EComp e qs)
  rename env (ESectR e op)         = liftM (flip ESectR (renE env op)) (rename env e) 
  rename env (ESectL op e)         = liftM (ESectL (renE env op)) (rename env e)
  rename env (ESelect e l)         = liftM (flip ESelect (renL env l)) (rename env e) 
  rename env (EAct v ss)           = liftM (EAct (fmap (renE env) v)) (renameS (unvoidAll env) (shuffleS ss))
  rename env (EReq v ss)           = liftM (EReq (fmap (renE env) v)) (renameS (unvoidAll env) (shuffleS ss))
  rename env (EDo (Just v) Nothing ss)
                                   = do env1 <- extRenSelf env v
                                        liftM (EDo (Just (renE env1 v)) Nothing) (renameS (unvoidAll env1) (shuffleS ss))
  rename env (ETempl (Just v) Nothing ss)
                                   = do env1 <- extRenSelf env v
                                        env2 <- setRenS env1 st
                                        liftM (ETempl (Just (renE env2 v)) Nothing) (renameS env2 (shuffleS ss))
    where st                       = assignedVars ss
  rename env (EAfter e1 e2)        = liftM2 EAfter (rename env e1) (rename env e2)
  rename env (EBefore e1 e2)       = liftM2 EBefore (rename env e1) (rename env e2)

renRec env (Just (n, t))           = Just (renT env n, t)
renRec env Nothing                 = Nothing

instance Rename Field where
  rename env (Field l e)           = liftM (Field (renL env l)) (rename env e)

instance Rename (Rhs Exp) where
  rename env (RExp e)              = liftM RExp (rename env e)
  rename env (RGrd gs)             = liftM RGrd (rename env gs)
  rename env (RWhere e bs)         = do env' <- extRenE env (bvars bs)
                                        liftM2 RWhere (rename env' e) (rename env' (shuffleB bs))


instance Rename (GExp Exp) where
  rename env (GExp qs e)           = do (qs,e) <- renameQ env qs e
                                        return (GExp qs e)


renameQ env [] e0                  = do e0 <- rename env e0
                                        return ([], e0)
renameQ env (QExp e : qs) e0       = do e <- rename env e
                                        (qs,e0) <- renameQ env qs e0
                                        return (QExp e : qs, e0)
renameQ env (QGen p e : qs) e0     = do e <- rename env e
                                        env' <- extRenE env (pvars p)
                                        p <- rename env' p
                                        (qs,e0) <- renameQ env' qs e0
                                        return (QGen p e : qs, e0)
renameQq env (QLet bs : qs) e0     = do env' <- extRenE env (bvars bs)
                                        bs <- rename env' bs
                                        (qs,e0) <- renameQ env' qs e0
                                        return (QLet bs : qs, e0)


instance Rename (Alt Exp) where
  rename env (Alt  p rh)           = do env' <- extRenE env (pvars p)
                                        liftM2 Alt (rename env' p) (rename env' rh) 


renameS env []                     = return []
renameS env [SRet e]               = liftM (:[]) (liftM SRet (rename env e))
renameS env (SExp e : ss)          = liftM2 (:) (liftM SExp (rename env e)) (renameS env ss)
renameS env (SGen p e : ss)        = do env' <- extRenE env (pvars p)
                                        liftM2 (:) (liftM2 SGen (rename env' p) (rename env e)) (renameS env' ss)
renameS env ss@(SBind _ : _)       = do env' <- extRenE env (bvars ss1)
                                        liftM2 (++) (mapM (renameB env') ss1) (renameS env' ss2)
  where (ss1,ss2)                  = span isSBind ss
        renameB env (SBind b)      = liftM SBind (rename env b)
renameS env (SAss p e : ss)
  | not (null illegal)             = fail ("Unknown state variable: " ++ showids illegal)
  | otherwise                      = liftM2 (:) (liftM2 SAss (rename (unvoidAll env) p) (rename env e)) (renameS (unvoid (pvars p) env) ss)
  where illegal                    = pvars p \\ stateVars env



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

