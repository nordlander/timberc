module Stuff where

import List(sort)
import Common
import Syntax


dostuff (Module c ds)           = Module c (stuff (selEnv ds) ds)

{-
    This module implements Record Stuffing; i.e. replacing ".." in record patterns and expressions 
    by sequences of bindings of the form "x=x" for missing record fields x.  Also adds type tag to 
    anonymous record expressions/patterns.
-}

-- The selector environment --------------------------------------------------------------------------------------

type SelEnv                     = Map Name [Name]

selEnv ds                       = map transClose recEnv
  where recEnv                  = [ (c,(map type2head ts, sort (concat (map sels ss)))) | DRec _ c _ ts ss <- ds ]
        sels (Sig vs _)         = vs
        transClose (c,(cs,ss))  = (c, ss ++ concat (map (selectors recEnv [c]) cs))
        selectors re cs0 c
          | c `elem` cs0        = error ("Circular record dependecies: " ++ showids (c:cs0))
          | otherwise           = case lookup c re of
                                    Just (cs,ss) -> ss ++ concat (map (selectors re (c:cs0)) cs)
                                    Nothing      -> error ("Unknown record constructor: " ++ show c)


selsFromType se c               = case lookup c se of
                                    Just ss -> ss
                                    Nothing -> error ("Unknown record constructor: " ++ show c)


typeFromSels [] ss              = error ("No record type with selectors " ++ showids (map chopSel ss))
typeFromSels  ((c,ss'):se) ss
  | ss == ss'                   = c
  | otherwise                   = typeFromSels se ss



-- Stuffing -------------------------------------------------------------------------------------------

class Stuff a where
    stuff :: SelEnv -> a -> a

instance Stuff a => Stuff [a] where
    stuff se                    = map (stuff se)

instance Stuff Decl where
    stuff se (DBind b)          = DBind (stuff se b)
    stuff se d                  = d

instance Stuff Bind where
    stuff se (BEqn lh rh)       = BEqn (stuff se lh) (stuff se rh)
    stuff se b                  = b

instance Stuff Lhs where
    stuff se (LFun v ps)        = LFun v (stuff se ps)
    stuff se (LPat p)           = LPat (stuff se p)

instance Stuff a => Stuff (Rhs a) where
    stuff se (RExp e)           = RExp (stuff se e)
    stuff se (RGrd gs)          = RGrd (stuff se gs)
    stuff se (RWhere e bs)      = RWhere (stuff se e) (stuff se bs)

instance Stuff a => Stuff (GExp a) where
    stuff se (GExp qs e)        = GExp (stuff se qs) (stuff se e)

instance Stuff Qual where
    stuff se (QExp e)           = QExp (stuff se e)
    stuff se (QGen p e)         = QGen (stuff se p) (stuff se e)
    stuff se (QLet bs)          = QLet (stuff se bs)

instance Stuff Exp where
    stuff se (ERec Nothing fs)  = ERec (Just (c,True)) (stuff se fs)
      where c                   = typeFromSels se (sort (bvars fs))
    stuff se (ERec (Just (c,all)) fs)
      | all && not (null miss)  = error ("Missing selectors in record: " ++ showids (map chopSel miss))
      | otherwise               = ERec (Just (c,True)) (fs' ++ stuff se fs)
      where miss                = selsFromType se c \\ bvars fs
            fs'                 = map (\s -> Field s (EVar (chopSel s))) miss

    stuff se (EAp e1 e2)        = EAp (stuff se e1) (stuff se e2)
    stuff se (ETup es)          = ETup (stuff se es)
    stuff se (EList es)         = EList (stuff se es)
    stuff se (ESig e t)         = ESig (stuff se e) t
    stuff se (ELam ps e)        = ELam (stuff se ps) (stuff se e)
    stuff se (ECase e as)       = ECase (stuff se e) (stuff se as)
    stuff se (EIf e1 e2 e3)     = EIf (stuff se e1) (stuff se e2) (stuff se e3)
    stuff se (ENeg e)           = ENeg (stuff se e)
    stuff se (ESeq e1 e2 e3)    = ESeq (stuff se e1) (stuff se e2) (stuff se e3)
    stuff se (EComp e qs)       = EComp (stuff se e) (stuff se qs)
    stuff se (ESectR e op)      = ESectR (stuff se e) op
    stuff se (ESectL op e)      = ESectL op (stuff se e)
    stuff se (ESelect e s)      = ESelect (stuff se e) s
    stuff se (EDo ss)           = EDo (stuff se ss)
    stuff se (EAct v ss)        = EAct v (stuff se ss)
    stuff se (EReq v ss)        = EReq v (stuff se ss)
    stuff se (ETempl v ss)      = ETempl v (stuff se ss)
    stuff se (EAfter e1 e2)     = EAfter (stuff se e1) (stuff se e2)
    stuff se (EBefore e1 e2)    = EBefore (stuff se e1) (stuff se e2)

    stuff se e                  = e

instance Stuff Field where
    stuff s (Field l e)         = Field l (stuff s e)

instance Stuff (Maybe Exp) where
    stuff se Nothing            = Nothing
    stuff se (Just e)           = Just (stuff se e)

instance Stuff Stmt where
    stuff se (SExp e)           = SExp (stuff se e)
    stuff se (SRet e)           = SRet (stuff se e)
    stuff se (SGen p e)         = SGen (stuff se p) (stuff se e)
    stuff se (SBind b)          = SBind (stuff se b)
    stuff se (SAss p e)         = SAss (stuff se p) (stuff se e)
    stuff se (SForall qs ss)    = SForall (stuff se qs) (stuff se ss)
    stuff se (SWhile e ss)      = SWhile (stuff se e) (stuff se ss)
    stuff se (SIf e ss)         = SIf (stuff se e) (stuff se ss)
    stuff se (SElsif e ss)      = SElsif (stuff se e) (stuff se ss)
    stuff se (SElse ss)         = SElse (stuff se ss)
    stuff se (SCase e as)       = SCase (stuff se e) (stuff se as)


instance Stuff a => Stuff (Alt a) where
    stuff se (Alt p e)          = Alt (stuff se p) (stuff se e)



