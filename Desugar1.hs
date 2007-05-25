module Desugar1 where

import List(sort)
import Monad
import Common
import Syntax
import Depend
import PP

desugar1 e0 (Module c is ds ps)  = do let (env,expInfo) = mkEnv c (ds ++ ps) e0
--                                      tr show(sels env))
                                      ds' <- dsDecls env ds
                                      ps' <- dsDecls env ps
                                      return (Module c is ds' ps',expInfo)

{-
    This module performs desugaring transformations that must be performed before the renaming pass:
    - Implements Record Stuffing; i.e. replacing ".." in record patterns and expressions 
      by sequences of bindings of the form "x=x" for missing record fields x.  
    - Adds type tag to anonymous record expressions/patterns.
    - Makes the "self" variable explicit.
    - Checks validity of return statement
    - Replaces prefix expression statement with dummy generator statement
    - Replaces if/case statements with corresponding expressions forms
     - Checks the restricted command syntax of template bodies
    - Unfolds use of type synonyms
-}

-- The selector environment --------------------------------------------------------------------------------------

data Env                        = Env { sels :: Map Name [Name], 
                                        self :: Maybe Name , 
                                        selSubst :: Map Name Name,
                                        modName :: Maybe String,
                                        isPat :: Bool,
                                        tsyns :: Map Name ([Name],Type)} deriving Show


env0 ss                       = Env { sels = [], self = Nothing, selSubst = [], modName = Nothing, isPat = False, tsyns = ss }


mkEnv c ds (rs,rn,ss)           = (env {sels = map transClose recEnv, modName = Just(str c), selSubst = rnSels },
                                   (closeRecEnvLoc,tsynsLoc))
  where (env,ssLoc)             = tsynE (env0 ss) [] tsynDecls
        tsynsLoc                = map (\(n,y) -> (n {fromMod = Just (str c)},y)) ssLoc
        recEnvLoc               = [ (c,(map type2head ts, concat (map sels ss))) | DRec _ c _ ts ss <- ds ] 
        recEnvImp               = zip ns (zip (repeat []) ss) where (ns,ss) = unzip rs
        closeRecEnvLoc          = map transClose recEnvLoc
        closeRecEnvImp          = map transClose recEnvImp
        recEnv                  = recEnvLoc ++ recEnvImp
        selsLocQual             = concatMap (snd . snd) recEnvLoc
        selsLoc                 = map (mName  Nothing) selsLocQual
        rnSels                  = zip selsLoc selsLocQual ++ zip selsLocQual selsLocQual ++ rn
        sels (Sig vs _)         = map (mName (Just (str c))) vs
        transClose (c,(cs,ss))  = (c, sort (ss ++ nub(concat (map (selectors [c]) cs))))
        selectors cs0 c
          | c `elem` cs0        = error ("Circular record dependencies: " ++ showids (c:cs0))
          | otherwise           = case lookup c recEnv of
                                    Just (cs,ss) -> ss ++ concat (map (selectors (c:cs0)) cs)
                                    Nothing      -> error ("Unknown record constructor: " ++ show c)

        tsynDecls                
          | not (null dups)     = error ("Duplicate type synonym declarations for "++showids dups)
          | otherwise           = topSort1 "Mutually recursive type synonyms " showids (tyCons . snd) syns
        syns                    = [(c,(vs,t)) | DType c vs t <- ds]
        dups                    = duplicates (map fst syns)


tsynE env ls []                 = (env,ls)
tsynE env ls ((c,(vs,t)) : ps) 
    | c `elem` tyCons t         = error ("Type synonym "++show c++" is recursive")
    | otherwise                 = tsynE env {tsyns =p : tsyns env} (p:ls) ps
 where p                        =  (c,(vs,ds1 env t))

selsFromType env c              = case lookup c (sels env) of
                                    Just ss -> ss
                                    Nothing -> if fromMod c == modName env
                                               then selsFromType env (c {fromMod = Nothing})
                                               else error ("Unknown record constructor: " ++ show c)


typeFromSels env ss             = f (sels env) ss
  where f [] ss                 = error ("No record type with selectors " ++ showids ss)
        f  ((c,ss'):se) ss
          | ss == ss'           = c
          | otherwise           = f se ss


haveSelf env                    = self env /= Nothing

tSubst env c ts                 = case lookup c (tsyns env) of
                                     Nothing -> foldl TAp (TCon c) (ds1 env ts)
                                     Just (vs,t)
                                       | length vs > length ts -> error ("Type synonym "++show c++" not fully applied")
                                       | otherwise -> foldl TAp (subst (zip vs (take (length vs) ts1)) t) 
                                                                (drop (length vs) ts1)
  where ts1                     = ds1 env ts

ren env cs                      = map ren' cs     
  where ren' c                  = case lookup c (selSubst env) of
                                     Nothing -> error ("Unknown record selector: " ++ show c)
                                     Just c' -> c'

patEnv env = env {isPat = True}
-- Desugaring -------------------------------------------------------------------------------------------

dsDecls env (DInst t bs : ds)   = do w <- newName instanceSym
                                     s <- renaming xs
                                     let bs' = map (substB s) bs
                                         r   = ERec (Just (type2head t,True)) (map mkField s)
                                     dsDecls env (DPSig w t : DBind (BEqn (LFun w []) (RWhere (RExp r) bs')) : ds)
  where mkField (x,x')          = Field x (EVar x')
        xs                      = nub (bvars bs)
        substB s (BSig vs t)    = BSig (substVars s vs) t
        substB s (BEqn lh rh)   = BEqn (substL s lh) rh
        substL s (LPat p)       = LPat (subst (mapSnd EVar s) p)
        substL s (LFun v ps)    = LFun (substVar s v) ps
dsDecls env (DType c vs t : ds)  = liftM (DType c vs (ds1 env t) :) (dsDecls env ds)
dsDecls env (DData c vs ss cs : ds) = liftM (DData c vs (ds1 env ss) (ds1 env cs) :) (dsDecls env ds)
dsDecls env (DRec b c vs ss ss' : ds) = liftM (DRec b c vs (ds1 env ss) (ds1 env ss') :) (dsDecls env ds)
dsDecls env (DPSig n t : ds)    = liftM (DPSig n (ds1 env t) :) (dsDecls env ds)
dsDecls env (DBind b : ds)      = liftM (DBind (ds1 env b) :) (dsDecls env ds)
dsDecls env (d : ds)            = liftM (d :) (dsDecls env ds)
dsDecls env []                  = return []


class Desugar1 a where
    ds1 :: Env -> a -> a

instance Desugar1 a => Desugar1 [a] where
    ds1 env                     = map (ds1 env)

instance Desugar1 a => Desugar1 (Maybe a) where
    ds1 env Nothing             = Nothing
    ds1 env (Just a)            = Just (ds1 env a)

instance Desugar1 Constr where
    ds1 env (Constr c ts ps)    = Constr c (ds1 env ts) (ds1 env ps)

instance Desugar1 Sig where
    ds1 env (Sig vs t)          = Sig vs (ds1 env t)

instance Desugar1 Type where
    ds1 env t@(TAp t1 t2)       = case tFlat t of
                                     (TCon c,ts) -> tSubst env c ts
                                     _ -> TAp (ds1 env t1) (ds1 env t2)
    ds1 env (TQual t ps)        = TQual (ds1 env t) (ds1 env ps)
    ds1 env (TSub t1 t2)        = TSub (ds1 env t1) (ds1 env t2)
    ds1 env (TList t)           = TList (ds1 env t)
    ds1 env (TTup ts)           = TTup (ds1 env ts)
    ds1 env (TFun ts t)         = TFun (ds1 env ts) (ds1 env t)
    ds1 env (TCon c)            = tSubst env c []
    ds1 env t                   = t

instance Desugar1 Pred where
    ds1 env (PType t)           = PType (ds1 env t)
    ds1 env p                   = p
instance Desugar1 Decl where
    ds1 env (DBind b)           = DBind (ds1 env b)
    ds1 env d                   = d

instance Desugar1 Bind where
    ds1 env (BEqn lh rh)        = BEqn (ds1 env lh) (ds1 env rh)
    ds1 env (BSig vs t)         = BSig vs (ds1 env t)

instance Desugar1 Lhs where
    ds1 env (LFun v ps)         = LFun v (ds1 (patEnv env) ps)
    ds1 env (LPat p)            = LPat (ds1 (patEnv env) p)

instance Desugar1 (Rhs Exp) where
    ds1 env (RExp e)            = RExp (ds1 env e)
    ds1 env (RGrd gs)           = RGrd (ds1 env gs)
    ds1 env (RWhere e bs)       = RWhere (ds1 env e) (ds1 env bs)

instance Desugar1 (GExp Exp) where
    ds1 env (GExp qs e)         = GExp (ds1 env qs) (ds1 env e)

instance Desugar1 Qual where
    ds1 env (QExp e)            = QExp (ds1 env e)
    ds1 env (QGen p e)          = QGen (ds1 (patEnv env) p) (ds1 env e)
    ds1 env (QLet bs)           = QLet (ds1 env bs)

instance Desugar1 Exp where
    ds1 env (ERec Nothing fs)
      | not (null dups)            = error ("Duplicate field definitions in record: " ++ showids dups)
      | otherwise                  = ERec (Just (c,True)) (ds1 env fs)
      where c                      = typeFromSels env (sort (ren env (bvars fs)))
            dups                   = duplicates (bvars fs)
    ds1 env (ERec (Just (c,all)) fs)
      | not (null dups)            = error ("Duplicate field definitions in record: " ++ showids dups)
      | all && not (null miss)     = error ("Missing selectors in record: " ++ showids miss)
      | otherwise                  = ERec (Just (c,True)) (fs' ++ ds1 env fs)
      where miss                   = ren env (selsFromType env c) \\ ren env (bvars fs)
            dups                   = duplicates (ren env (bvars fs))
            fs'                    = map (\s -> Field (tag0 (mkLocal s)) (EVar (mName Nothing (tag0 s)))) miss
            mkLocal s
              |fromMod s == modName env = mName Nothing s
              |otherwise           = s
    ds1 env (ELet bs e)            = ELet (ds1 env bs) (ds1 env e)
    ds1 env (EAp e1 e2)            = EAp (ds1 env e1) (ds1 env e2)
    ds1 env (ETup es)              = ETup (ds1 env es)
    ds1 env (EList es)             = EList (ds1 env es)
    ds1 env (ESig e t)             = ESig (ds1 env e) (ds1 env t)
    ds1 env (ELam ps e)            = ELam (ds1 (patEnv env) ps) (ds1 env e)
    ds1 env (ECase e as)           = ECase (ds1 env e) (ds1 env as)
    ds1 env (EIf e1 e2 e3)         = EIf (ds1 env e1) (ds1 env e2) (ds1 env e3)
    ds1 env (ENeg e)               = ENeg (ds1 env e)
    ds1 env (ESeq e1 e2 e3)        = ESeq (ds1 env e1) (ds1 env e2) (ds1 env e3)
    ds1 env (EComp e qs)           = EComp (ds1 env e) (ds1 env qs)
    ds1 env (ESectR e op)          = ESectR (ds1 env e) op
    ds1 env (ESectL op e)          = ESectL op (ds1 env e)
    ds1 env (ESelect e s)          = ESelect (ds1 env e) s
    ds1 env e@(ELit (LInt n))
      | isPat env                  = e
      | otherwise                  = EAp (EVar (name (0,0) "fromInt")) e 
     -- need a location to avoid being considered generated when C identifiers are rendered.

    ds1 env (ETempl Nothing t ss)  = ds1 env (ETempl (Just (name0 "self")) (ds1 env t) ss)
    ds1 env (EDo Nothing t ss)
      | haveSelf env               = ds1 env (EDo (self env) (ds1 env t) ss)
      | otherwise                  = ds1 env (EDo (Just (name0 "self")) (ds1 env t) ss)
    ds1 env (EAct Nothing ss)
      | haveSelf env               = ds1 env (EAct (self env) ss)
      | otherwise                  = error "Illegal action"
    ds1 env (EReq Nothing ss)
      | haveSelf env               = ds1 env (EReq (self env) ss)
      | otherwise                  = error "Illegal request"

    ds1 env (ETempl v t ss)      = ETempl v t (ds1T (env{self=v}) ss)
    ds1 env (EDo v t ss)         = EDo v t (ds1S (env{self=v}) ss)
    ds1 env (EAct v ss)          = EAct v [SExp (EDo v Nothing (ds1S env ss))]
    ds1 env (EReq v ss)          = EReq v [SExp (EDo v Nothing (ds1S env ss))]

    ds1 env (EAfter e1 e2)       = EAfter (ds1 env e1) (ds1 env e2)
    ds1 env (EBefore e1 e2)      = EBefore (ds1 env e1) (ds1 env e2) 

    ds1 env e                    = e

instance Desugar1 (Alt Exp) where
    ds1 env (Alt p rh)           = Alt (ds1 (patEnv env) p) (ds1 env rh)

instance Desugar1 Field where
    ds1 env (Field l e)          = Field l (ds1 env e)

ds1S env []                      = []
ds1S env [SExp e]                = [SExp (ds1 env e)]
ds1S env (SExp e : ss)           = SGen EWild (ds1 env e) : ds1S env ss
ds1S env [SRet e]                = [SRet (ds1 env e)]
ds1S env (SRet e : ss)           = error "Illegal return"
ds1S env (SGen p e : ss)         = SGen (ds1 (patEnv env) p) (ds1 env e) : ds1S env ss
ds1S env (SBind b : ss)          = SBind (ds1 env b) : ds1S env ss
ds1S env (SAss p e : ss)         = SAss (ds1 (patEnv env) p) (ds1 env e) : ds1S env ss
ds1S env (SCase e as : ss)       = ds1S env (SExp (ECase e (map doAlt as)) : ss)
  where doAlt (Alt p r)          = Alt (ds1 (patEnv env) p) (doRhs r)
        doRhs (RExp ss)          = RExp (EDo (self env) Nothing ss)
        doRhs (RGrd gs)          = RGrd (map doGrd gs)
        doRhs (RWhere r bs)      = RWhere (doRhs r) bs
        doGrd (GExp qs ss)       = GExp qs (EDo (self env) Nothing ss)
ds1S env (SIf e ss' : ss)        = doIf (EIf e (EDo (self env) Nothing ss')) ss
  where doIf f (SElsif e ss':ss) = doIf (f . EIf e (EDo (self env) Nothing ss')) ss
        doIf f (SElse ss':ss)    = ds1S env (SExp (f (EDo (self env) Nothing ss')) : ss)
        doIf f ss                = ds1S env (SExp (f (EDo (self env) Nothing [])) : ss)
ds1S env (SElsif e ss : _)       = error "Illegal elsif"
ds1S env (SElse ss : _)          = error "Illegal else"
ds1S env (SForall q ss' : ss)    = error "forall stmt not yet implemented"
ds1S env (SWhile e ss' : ss)     = error "while stmt not yet implemented"


ds1T env [SRet e]                = [SRet (ds1 env e)]
ds1T env (SBind b : ss)          = SBind (ds1 env b) : ds1T env ss
ds1T env (SAss p e : ss)         = SAss (ds1 (patEnv env) p) (ds1 env e) : ds1T env ss
ds1T env (SGen p e : ss)         = SGen (ds1 (patEnv env) p) (ds1 env e) : ds1T env ss           -- temporary
ds1T env ss                      = error ("Illegal statement in template: " ++ show ss)

-- Printing --------

instance Pr (Module,([(Name, [Name])], [(Name, ([Name], Type))])) where
 
   pr (m,_) = pr m
