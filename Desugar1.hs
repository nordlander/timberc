{-# LANGUAGE FlexibleInstances #-}

module Desugar1 where

import List(sort, sortBy)
import Monad
import Common
import Syntax
import Depend
import PP

desugar1 e0 (Module c is ds ps)  = do let (env,expInfo) = mkEnv c (ds ++ ps) e0
--                                      tr show(sels env))
                                      ds' <- dsDecls env ds
                                      ps' <- dsDecls env {isPublic = False} ps
                                      return (Module c is ds' ps',expInfo)

{-
    This module performs desugaring transformations that must be performed before the renaming pass:
    - Implements Record Stuffing; i.e. replacing ".." in record patterns and expressions 
      by sequences of bindings of the form "x=x" for missing record fields x.  
    - Adds type tag to anonymous record expressions/patterns.
    - Makes the "self" variable explicit.
    - Checks validity of result statement
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
                                        isPublic :: Bool,
                                        tsyns :: Map Name ([Name],Type)
                                  } deriving Show


env0 ss                       = Env { sels = [], self = Nothing, selSubst = [], modName = Nothing, isPat = False, 
                                      isPublic = True, tsyns = ss }


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
        selsLoc                 = map dropMod selsLocQual
        rnSels                  = zip selsLoc selsLocQual ++ zip selsLocQual selsLocQual ++ rn
        sels (Sig vs _)         = map (qName (str c)) vs
        transClose (c,(cs,ss))  = (c, sort (ss ++ nub(concat (map (selectors [c]) cs))))
        selectors cs0 c
          | c `elem` cs0        = errorIds "Circular struct dependencies" (c:cs0)
          | otherwise           = case lookup c recEnv of
                                    Just (cs,ss) -> ss ++ concat (map (selectors (c:cs0)) cs)
                                    Nothing      -> errorIds "Unknown struct constructor"  [c]

        tsynDecls                
          | not (null dups)     = errorIds "Duplicate type synonym declarations" dups
          | otherwise           = case topSort (tyCons . snd) syns of
                                    Left ns     -> errorIds "Mutually recursive type synonyms" ns
                                    Right syns' -> syns'
        syns                    = [(c,(vs,t)) | DType c vs t <- ds]
        dups                    = duplicates (map fst syns)


tsynE env ls []                 = (env,ls)
tsynE env ls ((c,(vs,t)) : ps) 
    | c `elem` tyCons t         = errorIds "Type synonym is recursive" [c]
    | otherwise                 = tsynE env {tsyns =p : tsyns env} (p:ls) ps
 where p                        =  (c,(vs,ds1 env t))

selsFromType env c              = case lookup c (sels env) of
                                    Just ss -> ss
                                    Nothing -> if fromMod c == modName env
                                               then selsFromType env (c {fromMod = Nothing})
                                               else errorIds "Unknown struct constructor" [c]


typeFromSels env ss             = f (sels env) ss
  where f [] ss                 = errorIds "No struct type with selectors" ss
        f  ((c,ss'):se) ss
          | ss == ss'           = c
          | otherwise           = f se ss


haveSelf env                    = self env /= Nothing

tSubst env c ts                 = case lookup c (tsyns env) of
                                     Nothing -> foldl TAp (TCon c) ts1
                                     Just (vs,t)
                                       | length vs > length ts -> errorIds "Type synonym not fully applied" [c]
                                       | otherwise -> foldl TAp (subst (zip vs (take (length vs) ts1)) t) 
                                                                (drop (length vs) ts1)
  where ts1                     = ds1 env ts

ren env cs                      = map ren' cs     
  where ren' c                  = case lookup c (selSubst env) of
                                     Nothing -> errorIds "Unknown struct selector" [c]
                                     Just c' -> c'

patEnv env                      = env {isPat = True}

sortFields fs                   = sortBy cmp fs
  where cmp (Field l1 _) (Field l2 _) = compare (str l1) (str l2)

-- Desugaring -------------------------------------------------------------------------------------------

dsDecls env (DType c vs t : ds) = liftM (DType c vs (ds1 env t) :) (dsDecls env ds)
dsDecls env (DData c vs ss cs : ds) = liftM (DData c vs (ds1 env ss) (ds1 env cs) :) (dsDecls env ds)
dsDecls env (DRec b c vs ss ss' : ds) = liftM (DRec b c vs (ds1 env ss) (ds1 env ss') :) (dsDecls env ds)
dsDecls env (DPSig n t : ds)    = liftM (\ds -> DInstance [n] : DBind [BSig [n] (ds1 env t)] : ds) (dsDecls env ds)
dsDecls env (DInstance ws : ds) = liftM (DInstance ws :) (dsDecls env ds)
dsDecls env (DTClass ts : ds)   = liftM (DTClass ts :) (dsDecls env ds)
dsDecls env (DDefault ts : ds)  = liftM (DDefault (ds1 env ts) :) (dsDecls env ds)
dsDecls env ds@(DBind _ : _)    = dsDs [] ds
  where dsDs bs (DBind bs':ds)  = dsDs (bs++bs') ds
        dsDs bs ds              = liftM (DBind (ds1 env bs) :) (dsDecls env ds)
dsDecls env (d : ds)            = liftM (d :) (dsDecls env ds)
dsDecls env []                  = return []


class Desugar1 a where
    ds1 :: Env -> a -> a

instance Desugar1 a => Desugar1 [a] where
    ds1 env                     = map (ds1 env)

instance Desugar1 a => Desugar1 (Maybe a) where
    ds1 env Nothing             = Nothing
    ds1 env (Just a)            = Just (ds1 env a)

instance Desugar1 (Default Type) where
   ds1 env (Default _ a b)      = Default (isPublic env) a b
   ds1 env (Derive v t)         = Derive v (ds1 env t)

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
    ds1 env (DBind bs)          = DBind (ds1 env bs)
    ds1 env d                   = d

instance Desugar1 Bind where
    ds1 env (BEqn lh rh)        = BEqn (ds1 env lh) (ds1 env rh)
    ds1 env (BSig vs t)         = BSig vs (ds1 env t)

instance Desugar1 Lhs where
    ds1 env (LFun v ps)         = LFun v (ds1 (patEnv env) ps)
    ds1 env (LPat (EVar x))     = LFun x []
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
--    ds1 env e@(EVar _)             = e
    ds1 env (ERec Nothing fs)
      | not (null dups)            = errorIds "Duplicate field definitions in struct" dups
      | otherwise                  = ERec (Just (c,True)) (sortFields (ds1 env fs))
      where c                      = typeFromSels env (sort (ren env (bvars fs)))
            dups                   = duplicates (bvars fs)
    ds1 env (ERec (Just (c,all)) fs)
      | not (null dups)            = errorIds "Duplicate field definitions in struct" dups
      | all && not (null miss)     = errorIds "Missing selectors in struct" miss
      | otherwise                  = ERec (Just (c,True)) (sortFields (fs' ++ ds1 env fs))
      where miss                   = ren env (selsFromType env c) \\ ren env (bvars fs)
            dups                   = duplicates (ren env (bvars fs))
            fs'                    = map (\s -> Field (tag0 (mkLocal s)) (EVar (dropMod (tag0 s)))) miss
            mkLocal s
              | fromMod s == modName env = dropMod s
              | otherwise          = s
    ds1 env (EBStruct _ _ bs)      = EBStruct (Just c) labs (ds1 env bs)
      where labs                   = selsFromType env c
            c                      = typeFromSels env (sort (ren env (bvars bs)))
    ds1 env (ELet bs e)            = ELet (ds1 env bs) (ds1 env e)
    ds1 env (EAp e1 e2)            = EAp (ds1 env e1) (ds1 env e2)
    ds1 env (ETup es)              = ETup (ds1 env es)
    ds1 env (EList es)             = EList (ds1 env es)
    ds1 env (ESig e t)             = ESig (ds1 env e) (ds1 env t)
    ds1 env (ELam ps e)            = ELam (ds1 (patEnv env) ps) (ds1 env e)
    ds1 env (ECase e as)           = ECase (ds1 env e) (ds1 env as)
    ds1 env (EIf e1 e2 e3)         = EIf (ds1 env e1) (ds1 env e2) (ds1 env e3)
    ds1 env (ENeg (ELit (LInt p i))) = ELit (LInt p (-i))
    ds1 env (ENeg (ELit (LRat p r))) = ELit (LRat p (-r))
    ds1 env (ENeg e)               = EAp (EVar (name' "negate" e)) (ds1 env e)
    ds1 env (ESeq e1 Nothing e3)   = EAp (EAp (EVar (name' "enumFromTo" e1)) (ds1 env e1)) (ds1 env e3)
    ds1 env (ESeq e1 (Just e2) e3) = EAp (EAp (EAp (EVar (name' "enumFromThenTo" e1)) (ds1 env e1)) (ds1 env e2)) (ds1 env e3)
    ds1 env (EComp e qs)           = EComp (ds1 env e) (ds1 env qs)
    ds1 env (ESectR e op)          = ESectR (ds1 env e) op
    ds1 env (ESectL op e)          = ESectL op (ds1 env e)
    ds1 env (ESelect e s)          = ESelect (ds1 env e) s
    ds1 env e@(ELit (LInt _ n))
      | isPat env                  = e
      | otherwise                  = EAp (EVar (name' "fromInt" e)) e 

    ds1 env (ETempl Nothing t ss)  = ds1 env (ETempl (Just (name0 "self")) (ds1 env t) ss)
    ds1 env (EDo Nothing t ss)
      | haveSelf env               = ds1 env (EDo (self env) (ds1 env t) ss)
      | otherwise                  = ds1 env (EDo (Just (name0 "self")) (ds1 env t) ss)
    ds1 env e@(EAct Nothing ss)
      | haveSelf env               = ds1 env (EAct (self env) ss)
      | otherwise                  = errorTree "Action outside class" e
    ds1 env e@(EReq Nothing ss)
      | haveSelf env               = ds1 env (EReq (self env) ss)
      | otherwise                  = errorTree "Request outside class" e

    ds1 env (ETempl v t ss)      = ETempl v t (ds1T (env{self=v}) [] ss)
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


ds1S env []                      = [SRet (ECon (prim UNITTERM))]
ds1S env [SExp e]                = [SExp (ds1 env e)]
ds1S env (SExp e : ss)           = SGen EWild (ds1 env e) : ds1S env ss
ds1S env [SRet e]                = [SRet (ds1 env e)]
ds1S env (s@(SRet _) : ss)       = errorTree "Result statement must be last in sequence" s
ds1S env (SGen p e : ss)         = SGen (ds1 (patEnv env) p) (ds1 env e) : ds1S env ss
ds1S env ss@(SBind _ : _)        = dsBs [] ss
  where dsBs bs (SBind bs' : ss) = dsBs (bs++bs') ss
        dsBs bs ss               = SBind (ds1 env bs) : ds1S env ss
ds1S env (SAss p e : ss)         = dsAss p e : ds1S env ss
  where dsAss (EAp (EAp (EVar (Prim IndexArray _)) a) i) e
                                 = dsAss a (EAp (EAp (EAp (EVar (prim UpdateArray)) a) i) e)
        dsAss p e                = SAss (ds1 env p) (ds1 env e)
        {-
            a|x|y|z := e
            a|x|y   := a|x|y \\ (z,e)
            a|x     := a|x \\ (y, a|x|y \\ (z,e))
            a       := a \\ (x, a|x \\ (y, a|x|y \\ (z,e)))
        -}
ds1S env (SCase e as : ss)       = ds1S env (SExp (retComplete (ECase e (map doAlt as))) : ss)
  where doAlt (Alt p r)          = Alt (ds1 (patEnv env) p) (doRhs r)
        doRhs (RExp ss)          = RExp (eDo env ss)
        doRhs (RGrd gs)          = RGrd (map doGrd gs)
        doRhs (RWhere r bs)      = RWhere (doRhs r) bs
        doGrd (GExp qs ss)       = GExp qs (eDo env ss)
ds1S env (SIf e ss' : ss)        = doIf (EIf e (eDo env ss')) ss
  where doIf f (SElsif e ss':ss) = doIf (f . EIf e (eDo env ss')) ss
        doIf f (SElse ss':ss)    = ds1S env (SExp (retComplete (f (eDo env ss'))) : ss)
        doIf f ss                = doIf f (SElse [] : ss)
ds1S env (s@(SElsif _ _) : _)    = errorTree "elsif without corresponding if" s
ds1S env (s@(SElse _) : _)       = errorTree "else without corresponding if" s
ds1S env (SForall q ss' : ss)    = ds1S env (SExp (ds1Forall env q ss') : ss) -- error "forall stmt not yet implemented"
ds1S env (SWhile e ss' : ss)     = internalError0 "while stmt not yet implemented"

ds1Forall env [] ss              = eDo env ss
ds1Forall env (QLet bs : qs) ss  = ELet bs (eDo env [SForall qs ss])
ds1Forall env (QGen p (ESeq e1 Nothing e3) : qs) ss
                                 = EAp (EAp (EAp (EVar (name' "forallSeq" p)) (ELam [p] (eDo env [SForall qs ss]))) e1) e3
ds1Forall env (QGen p (ESeq e1 (Just e2) e3) : qs) ss
                                 = EAp (EAp (EAp (EAp (EVar (name' "forallSeq1" p)) (ELam [p] (eDo env [SForall qs ss]))) e1) e2) e3
ds1Forall env (QGen p e : qs) ss = EAp (EAp (EVar (name' "forallList" p)) (ELam [p] (eDo env [SForall qs ss]))) e
ds1Forall env (QExp e : qs) ss   = EIf e (eDo env [])  (eDo env [SForall qs ss])

ds1T env asg [SRet e]            = reverse asg ++ [SRet (ds1 env e)]
ds1T env asg [s]                 = errorTree "Last statement in class must be result, not" s
ds1T env asg (s@(SRet _) : ss)   = errorTree "Result statement must be last in sequence" s
ds1T env asg ss@(SBind _ : _)    = dsBs [] ss
  where dsBs bs [s@(SBind _)]    = errorTree "Last statement in class must be result, not" s
        dsBs bs (SBind bs' : ss) = dsBs (bs++bs') ss
        dsBs bs ss               = SBind (ds1 env bs) : ds1T env asg ss
ds1T env asg (s@(SAss p e) : ss) = ds1T env (SAss (ds1 (patEnv env) p) (ds1 env e) : asg) ss
ds1T env asg (s : _)             = errorTree "Illegal statement in class: " s

retComplete e
  | hasRet e                     = e
  | otherwise                    = addRet e
  where hasRet (EIf _ e1 e2)     = hasRet e1 || hasRet e2
        hasRet (EDo _ _ ss)      = hasRetS ss
        hasRet (ECase e alts)    = any hasRetA alts
        hasRet _                 = True
        hasRetA (Alt p r)        = hasRetR r
        hasRetR (RExp e)         = hasRet e
        hasRetR (RGrd gs)        = any hasRetG gs
        hasRetR (RWhere r bs)    = hasRetR r
        hasRetG (GExp qs e)      = hasRet e
        hasRetS []               = False
        hasRetS [SRet (ECon (Prim UNITTERM _))]
                                 = False
        hasRetS [SRet e]         = True
        hasRetS (s:ss)           = hasRetS ss
        addRet (EIf e e1 e2)     = EIf e (addRet e1) (addRet e2)
        addRet (EDo v t ss)      = EDo v t (addRetS ss)
        addRet (ECase e alts)    = ECase e (map addRetA alts)
        addRet e                 = e
        addRetA (Alt p r)        = Alt p (addRetR r)
        addRetR (RExp e)         = RExp (addRet e)
        addRetR (RGrd gs)        = RGrd (map addRetG gs)
        addRetR (RWhere r bs)    = RWhere (addRetR r) bs
        addRetG (GExp qs e)      = GExp qs (addRet e)
        addRetS []               = [SRet (ECon (prim UNITTERM))]
        addRetS ss@[SRet _]      = ss
        addRetS (s:ss)           = s : addRetS ss


eDo env ss                       = EDo (self env) Nothing ss

maybeGen e []                    = [SExp e]
maybeGen e ss                    = SGen EWild e : ss

name' s  e                       = Name s 0 Nothing (noAnnot {location = loc (posInfo e)})
  where loc Unknown              = Nothing
        loc (Between p _)        = Just p

-- Printing --------

instance Pr (Module,([(Name, [Name])], [(Name, ([Name], Type))])) where
 
   pr (m,_) = pr m
