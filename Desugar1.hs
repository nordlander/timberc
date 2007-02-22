module Desugar1 where

import List(sort)
import Monad
import Common
import Syntax


desugar1 (Module c ds)          = do ds <- dsDecls (mkEnv ds) ds
                                     return (Module c ds)

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
-}

-- The selector environment --------------------------------------------------------------------------------------

data Env                        = Env { sels :: Map Name [Name], self :: Maybe Name }

mkEnv ds                        = Env { sels = map transClose recEnv, self = Nothing }
  where recEnv                  = [ (c,(map type2head ts, sort (concat (map sels ss)))) | DRec _ c _ ts ss <- ds ]
        sels (Sig vs _)         = vs
        transClose (c,(cs,ss))  = (c, ss ++ concat (map (selectors recEnv [c]) cs))
        selectors re cs0 c
          | c `elem` cs0        = error ("Circular record dependecies: " ++ showids (c:cs0))
          | otherwise           = case lookup c re of
                                    Just (cs,ss) -> ss ++ concat (map (selectors re (c:cs0)) cs)
                                    Nothing      -> error ("Unknown record constructor: " ++ show c)


selsFromType env c              = case lookup c (sels env) of
                                    Just ss -> ss
                                    Nothing -> error ("Unknown record constructor: " ++ show c)


typeFromSels env ss             = f (sels env) ss
  where f [] ss                 = error ("No record type with selectors " ++ showids ss)
        f  ((c,ss'):se) ss
          | ss == ss'           = c
          | otherwise           = f se ss


haveSelf env                    = self env /= Nothing


-- Desugaring -------------------------------------------------------------------------------------------

dsDecls env (DInst t bs : ds)   = do w <- newName instanceSym
                                     dsDecls env (DPSig w t : DBind (BEqn (LFun w []) (RWhere (RExp r) bs)) : ds)
  where r                       = ERec (Just (type2head t,True)) (map mkField (bvars bs))
        mkField v | isId v      = Field v (EVar v)
                  | otherwise   = error "Illegal symbol bindinging in instance declaration"
dsDecls env (DBind b : ds)      = liftM (DBind (ds1 env b) :) (dsDecls env ds)
dsDecls env (d : ds)            = liftM (d :) (dsDecls env ds)
dsDecls env []                  = return []


class Desugar1 a where
    ds1 :: Env -> a -> a

instance Desugar1 a => Desugar1 [a] where
    ds1 env                     = map (ds1 env)

instance Desugar1 Decl where
    ds1 env (DBind b)           = DBind (ds1 env b)
    ds1 env d                   = d

instance Desugar1 Bind where
    ds1 env (BEqn lh rh)        = BEqn (ds1 env lh) (ds1 env rh)
    ds1 env b                   = b

instance Desugar1 Lhs where
    ds1 env (LFun v ps)         = LFun v (ds1 env ps)
    ds1 env (LPat p)            = LPat (ds1 env p)

instance Desugar1 (Rhs Exp) where
    ds1 env (RExp e)            = RExp (ds1 env e)
    ds1 env (RGrd gs)           = RGrd (ds1 env gs)
    ds1 env (RWhere e bs)       = RWhere (ds1 env e) (ds1 env bs)

instance Desugar1 (GExp Exp) where
    ds1 env (GExp qs e)         = GExp (ds1 env qs) (ds1 env e)

instance Desugar1 Qual where
    ds1 env (QExp e)            = QExp (ds1 env e)
    ds1 env (QGen p e)          = QGen (ds1 env p) (ds1 env e)
    ds1 env (QLet bs)           = QLet (ds1 env bs)

instance Desugar1 Exp where
    ds1 env (ERec Nothing fs)
      | not (null dups)            = error ("Duplicate field definitions in record: " ++ showids dups)
      | otherwise                  = ERec (Just (c,True)) (ds1 env fs)
      where c                      = typeFromSels env (sort (bvars fs))
            dups                   = duplicates (bvars fs)
    ds1 env (ERec (Just (c,all)) fs)
      | not (null dups)            = error ("Duplicate field definitions in record: " ++ showids dups)
      | all && not (null miss)     = error ("Missing selectors in record: " ++ showids miss)
      | otherwise                  = ERec (Just (c,True)) (fs' ++ ds1 env fs)
      where miss                   = selsFromType env c \\ bvars fs
            dups                   = duplicates (bvars fs)
            fs'                    = map (\s -> Field s (EVar s)) miss
 
    ds1 env (ELet bs e)            = ELet (ds1 env bs) (ds1 env e)
    ds1 env (EAp e1 e2)            = EAp (ds1 env e1) (ds1 env e2)
    ds1 env (ETup es)              = ETup (ds1 env es)
    ds1 env (EList es)             = EList (ds1 env es)
    ds1 env (ESig e t)             = ESig (ds1 env e) t
    ds1 env (ELam ps e)            = ELam (ds1 env ps) (ds1 env e)
    ds1 env (ECase e as)           = ECase (ds1 env e) (ds1 env as)
    ds1 env (EIf e1 e2 e3)         = EIf (ds1 env e1) (ds1 env e2) (ds1 env e3)
    ds1 env (ENeg e)               = ENeg (ds1 env e)
    ds1 env (ESeq e1 e2 e3)        = ESeq (ds1 env e1) (ds1 env e2) (ds1 env e3)
    ds1 env (EComp e qs)           = EComp (ds1 env e) (ds1 env qs)
    ds1 env (ESectR e op)          = ESectR (ds1 env e) op
    ds1 env (ESectL op e)          = ESectL op (ds1 env e)
    ds1 env (ESelect e s)          = ESelect (ds1 env e) s

    ds1 env (ETempl Nothing t ss)  = ds1 env (ETempl (Just (name0 "self")) t ss)
    ds1 env (EDo Nothing t ss)
      | haveSelf env               = ds1 env (EDo (self env) t ss)
      | otherwise                  = ds1 env (EDo (Just (name0 "self")) t ss)
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
    ds1 env (Alt p rh)           = Alt (ds1 env p) (ds1 env rh)

instance Desugar1 Field where
    ds1 env (Field l e)          = Field l (ds1 env e)

instance Desugar1 (Maybe Exp) where
    ds1 env Nothing              = Nothing
    ds1 env (Just e)             = Just (ds1 env e)


ds1S env []                      = []
ds1S env [SExp e]                = [SExp (ds1 env e)]
ds1S env (SExp e : ss)           = SGen EWild (ds1 env e) : ds1S env ss
ds1S env [SRet e]                = [SRet (ds1 env e)]
ds1S env (SRet e : ss)           = error "Illegal return"
ds1S env (SGen p e : ss)         = SGen (ds1 env p) (ds1 env e) : ds1S env ss
ds1S env (SBind b : ss)          = SBind (ds1 env b) : ds1S env ss
ds1S env (SAss p e : ss)         = SAss (ds1 env p) (ds1 env e) : ds1S env ss
ds1S env (SCase e as : ss)       = ds1S env (SExp (ECase e (map doAlt as)) : ss)
  where doAlt (Alt p r)          = Alt p (doRhs r)
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
ds1T env (SAss p e : ss)         = SAss (ds1 env p) (ds1 env e) : ds1T env ss
ds1T env (SGen p e : ss)         = SGen (ds1 env p) (ds1 env e) : ds1T env ss           -- temporary
ds1T env ss                      = error ("Illegal statement in template: " ++ show ss)
